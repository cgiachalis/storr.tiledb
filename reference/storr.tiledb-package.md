# storr.tiledb: A TileDB Storage Driver for Storr

Provides a TileDB driver for 'storr' interface. A 'storr' is a content
addressed key value store with an optional caching layer; this package
contributes a new driver using TileDB Embedded storage engine as a
backend. In addition, storr.tiledb introduces a specialized TileDBStorr
class that replicates 'storr' API in order to improve speed and
efficiency while enhancing 'storr' with additional features, i.e.,
per-key notes and Time-To-Live (TTL) expiration timestamps as well as
support for asynchronous and in parallel writes.

## See also

Useful links:

- <https://github.com/cgiachalis/storr.tiledb>

- <https://cgiachalis.github.io/storr.tiledb/>

- Report bugs at <https://github.com/cgiachalis/storr.tiledb/issues>

## Author

**Maintainer**: Constantinos Giachalis
<26255858+cgiachalis@users.noreply.github.com>

Authors:

- Constantinos Giachalis <26255858+cgiachalis@users.noreply.github.com>
