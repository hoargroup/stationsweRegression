#!/usr/bin/env python
'''
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

This script will facilitate downloading of snow products over WEBDAV.
'''
import json
import os
import urllib2
import ConfigParser
import time
import re
from optparse import OptionParser
from datetime import datetime, timedelta
from HTMLParser import HTMLParser

# Configure which product types will be downloaded when none is specified
default_product_types = ['SCAG_NRT_ALL']

class ProductConfig(object):
    '''
    The TypeConfig class captures all the information pertinent to accessing
    SnowDS products over WebDAV. 
    '''
    def __init__(self, identifier, path, prefix, filenames, extensions, nrt, description=None):
        self._identifier = identifier
        self._description = description
        self._path = path
        self._prefix = prefix
        self._nrt = nrt
        if filenames is not None:
            if not isinstance(filenames, list):
                raise TypeError('filenames should be a list, not a %r' % filenames.__class__)
            self._filenames = filenames
        else:
            self.filenames = list()
        if extensions is not None:
            if not isinstance(extensions, list):
                raise TypeError('extensions should be a list, not a %r' % extensions.__class__)
            self._extensions = extensions
        else:
            self.extensions = list()
    @property
    def identifier(self):
        '''Gives an immutable unique name for to this config.'''
        return self._identifier
    @property
    def description(self):
        '''Provides an immutable human readable description.'''
        return self._description
    @property
    def path(self):
        '''The immutable root path under which products of this type are hosted.'''
        return self._path
    @property
    def prefix(self):
        '''The immutable prefix for the filename.'''
        return self._prefix
    @property
    def filenames(self):
        '''The immutable list of types of files to be included in this product config.'''
        return self._filenames
    @property
    def extensions(self):
        '''The immutable list of file extensions that are available for this product.'''
        return self._extensions
    @property
    def nrt(self):
        '''An immutable flag indicating if this is a near real time product.'''
        return self._nrt


def setup_cmdline_parser():
    '''Sets up the options available on the command line. Minimally the user
    should specify username, password, region (or tile_ids), and date range.
    Should also allow for type, output file, and mode (python, wget, or curl).

    >>> parser = setup_cmdline_parser()
    '''
    parser = OptionParser()
    parser.add_option('-i', '--ignore', action='store_true', dest='ignore',
        help='Ignore downloading files that are already present.', default=False)
    parser.add_option('-c', '--config', action='store', type='string',
        dest='config', help='''Config file to describe products and server config''',
        default='download.cfg')
    parser.add_option('-u', '--user', action='store', type='string',
        dest='user', help='''REQUIRED - Username assigned to you by SnowDS team''')
    parser.add_option('-p', '--passwd', action='store', type='string',
        dest='passwd', help='''REQUIRED - Password assigned to you by SnowDS team''')
    parser.add_option('-r', '--region', action='store', type='string',
        dest='region', help='''REQUIRED - Region of interest.''')
    parser.add_option('-t', '--types', action='store', type='string',
        default=None, dest='product_types', help='Product type to download '
        '(List of comma separated values).')
    parser.add_option('-s', '--start', action='store', type='string', 
        default=None, dest='start', help='REQUIRED - Provide a start date in the format '
        'YYYYDDD, where DDD is a 3-digit Day Of Year')
    parser.add_option('-e', '--end', action='store', type='string', 
        default=None, dest='end', help='Provide an end date in the format '
        'YYYYDDD, where DDD is a 3-digit Day Of Year. '
        'NOTE: This date is NOT INCLUSIVE for downloading tiles.')
    parser.add_option('-m', '--mode', action='store', type='string',
        default='python', dest='mode', help='Specifying "python" will tell the '
        'script to do the download, "wget" will output a list of wget commands, ' 
        'and "curl" will output a list of curl commands.  '
        'This will default to "python" and download the products')
    return parser

def validate_cmdline(parser, options, args):
    '''Checks the command line options'''
    if not options.user:
        parser.error('Username required.\n\nFor a complete list of parameters and arguments use the -h flag or --help')
    if not options.passwd:
        parser.error('Password required.\n\nFor a complete list of parameters and arguments use the -h flag or --help')
    if not options.region:
        parser.error('Region required.\n\nFor a complete list of parameters and arguments use the -h flag or --help')
    if not options.start:
        parser.error('Start date is required.\n\nFor a complete list of parameters and arguments use the -h flag or --help')

def download_file(base_uri, filepath, ignore=False):
    '''Will download and save the file using python urllib2. The file will be saved 
    to the same name as it has on the server and with the same relative pathing.'''
    url = base_uri + filepath
    try:
        data = urllib2.urlopen(url)
        dir = os.path.dirname(filepath)
        if not os.path.exists(dir):
            os.makedirs(dir)
        if ignore and os.path.isfile(filepath):
            print 'Skipping (already present): %s' % (filepath)
        else:
            f = open(filepath, 'w')
            f.write(data.read())
            print 'Downloaded: %s' % (filepath)
    except urllib2.HTTPError, ex:
        print 'Product not available: %s; (%s)' % (url, ex)
        time.sleep(1)
    except urllib2.URLError, ex:
        print 'Invalid URL: %s; (%s)' % (url, ex)
    except IOError, ex:
        print 'Cannot write: %s; (%s)' % (filepath, ex)

def setup_auth(realm, uri, user, passwd):
    '''Sets up the login for SnowDS WebDAV'''
    auth_handler = urllib2.HTTPDigestAuthHandler()
    auth_handler.add_password(realm=realm, uri=uri, user=user, passwd=passwd)
    opener = urllib2.build_opener(auth_handler)
    urllib2.install_opener(opener)

def daterange(start_date, end_date):
    '''Generates a set of year and doy from start_date up to but not including end_date'''
    for n in range((end_date - start_date).days):
        next_date = start_date + timedelta(n)
        year = next_date.strftime('%Y')
        doy = next_date.strftime('%j')
        yield year, doy

def generate_filepaths(base_uri, product_config, tiles, year, doy, extensions):
    '''Create a set of filepaths that align with the given product type. This will pick up
    all the tiles in the given region, for the given year, and doy, for the set of 
    extensions specified.'''
    filepaths = []
    try:
        # The mode part of the path for NRT processing is 'NRT' 
        # otherwise julian date of modis processing
        if product_config.nrt:
            processing = 'NRT'
        else:
            # Grab the date from the directory listin on webdav
            doy_url = base_uri + product_config.path + '/' + year + '/' + doy
            data = urllib2.urlopen(doy_url)
            dir_listing = data.read()
        for tile_id in tiles:
            if not product_config.nrt:
                # find offsets to julian date for tile
                index = dir_listing.find(tile_id)
                processing = dir_listing[index+11:index+24]
            if index != -1:
                for extension in extensions:
                    for filename in product_config.filenames:
                        filepaths.append('%s/%s/%s/%s%s%s.%s.005.%s%s.%s' % (product_config.path, 
                            year, doy, product_config.prefix, year, doy, tile_id, processing,
                            filename, extension))
            else:
                print '%s products are not available for: %s' % (product_config.identifier, doy_url)
    except urllib2.HTTPError, ex:
        print '%s products are not available for: %s (%s)' % (product_config.identifier, doy_url, ex)
        time.sleep(10)
    except urllib2.URLError, ex:
        print 'Invalid URL: %s (%s)' % (doy_url, ex)

    return filepaths

def product_urls(base_uri, product_config, tiles, start_date, end_date):
    # For caching valid doys for each year
    valid_doys = {}
    # Hit the server up for the directory lising on years
    data = urllib2.urlopen(base_uri + product_config.path)
    # Parse year lising for all years
    valid_years = set(re.findall('<a href="(.*?\d{4}.*?)/">.*?</a>', data.read()))
    for year, doy in daterange(start_date, end_date):
        if year in valid_years:
            if not valid_doys.has_key(year):
                # Get the product listing for the year
                data = urllib2.urlopen(base_uri + product_config.path + '/' + year)
                # Hit the server up for the directory lising on years
                # Cache them for later
                valid_doys[year] = set(re.findall('<a href="(.*?\d{3}.*?)/">.*?</a>',  data.read()))
            if doy in valid_doys[year]:
                path = product_config.path + '/' + year + '/' + doy
                # Get the product listing for the year, doy
                data = urllib2.urlopen(base_uri + path)
                expression = '<a href="(.*?(%s).*?(%s).*?(%s))">.*?</a>' % ('|'.join(tiles), 
                    '|'.join(product_config.filenames), '|'.join(product_config.extensions))
                for match in re.findall(expression, data.read()):
                    yield path + '/' + match[0]

def load_product_config(config, section):
    return ProductConfig(
                identifier=section,
                path=config.get(section, 'path'),
                prefix=config.get(section, 'prefix'),
                filenames=json.loads(config.get(section, 'filenames')),
                extensions=json.loads(config.get(section, 'extensions')),
                nrt=config.getboolean(section, 'nrt'),
                description=config.get(section, 'description')
                )

def main():
    # Handle command line
    parser = setup_cmdline_parser()
    (options, args) = parser.parse_args()
    validate_cmdline(parser, options, args)

    # Set defaults for parameters
    start_date = datetime.strptime(options.start,'%Y%j')
    end_date = datetime.strptime(options.end,'%Y%j') if options.end else datetime.today()
    product_types = options.product_types.split(',') if options.product_types else default_product_types 

    # Read in product type config
    config = ConfigParser.RawConfigParser()
    config.read(options.config)

    product_configs = []
    for product_type in product_types:
        product_configs.append(load_product_config(config, product_type))

    tiles = json.loads(config.get('REGIONS', options.region))

    base_uri = config.get('SERVER','snow_dav_url')
    realm = config.get('SERVER','snow_realm')
    # Setup login
    setup_auth(realm, base_uri, options.user, options.passwd)

    for product_config in product_configs:
        relative_urls = product_urls(base_uri, product_config, tiles, start_date, end_date)
        for relative_url in relative_urls:
            download_file(base_uri, relative_url, options.ignore)

if __name__ == '__main__':
    main()
