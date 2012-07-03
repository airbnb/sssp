==========================================
 s3p - software distribution backed by S3
==========================================

Synopsis
--------

.. code-block:: text

    s3p ...

Description
-----------

S3P is a proxy for S3 that can generate short-lived, signed URLs for stored
objects. By providing a server separate from S3 that can be placed behind an
authenticating proxy or firewall, S3P allows a variety of common security
mechanisms to be used to limit access to S3 objects over HTTP while taking
advantage of S3's considerable bandwidth and parallelism.

Use-cases for S3P include:

  * sharing of large files within an organization,

  * media service for public facing web applications,

  * distribution of internal software.

Options
-------

  ``-lolno``
    No LOLs are permitted with this option.

REST Interface
--------------

URLs in S3P point to one of two objects: an item or a listing. Items
correspond to S3 objects; a GET retrieves a signed redirect to the object.
Listings are a sequence of URLs, in ascending order; a GET retrieves the
listing as a plaintext document, one URL per line.

Signed redirects to items are, by default, good for ten seconds; but the time
can be specified with the ``t`` parameter, which accepts a number of seconds or
an ISO 8601 date. The signed redirect is always a 303 that points directly to
Amazon S3. If the ``nosign`` parameter is given, the redirect points back to
the S3P server; this is the identity for most URLs but can be useful when
working with wildcards (see below).

.. code-block:: text

  GET http://s3p.io/p/a/t/h         # Signed for the default time (10s).
  GET http://s3p.io/p/a/t/h?t=_n_s  # Signed for _n_ seconds.
  GET http://s3p.io/p/a/t/h?t=_t_   # Signed until _t_.
  GET http://s3p.io/p/a/t/h?nosign  # Just this URL again.

A PUT to an item sets the item's content; a PUT to a listing sets all the
items. Similarly, a DELETE to an item is singular while a DELETE to a listing
is plural.

URLs are divided syntactically in to listings and items. A URL ending with a
slash is always a listing.

.. code-block:: text

  GET http://s3p.io/raw    # Signed redirect to an object called raw.
  GET http://s3p.io/raw/   # Listing of items below the key `raw'.

To make it easier to work with versioned or timestamped assets, S3P supports a
``/hi`` and ``/lo`` meta-path. These correspond to the ASCIIbetically highest
and lowest (last and first) items, respectively.

.. code-block:: text

  GET http://s3p.io/raw/2010-04/mbox
  GET http://s3p.io/raw/2010-05/mbox
  GET http://s3p.io/raw/2010-06/mbox
  GET http://s3p.io/raw/2010-07/mbox

  # Retrieval with /hi and /lo.
  GET http://s3p.io/raw//hi/mbox  -303->  http://s3p.io/raw/2010-07/mbox
  GET http://s3p.io/raw//lo/mbox  -303->  http://s3p.io/raw/2010-04/mbox

The ``/hi`` and ``/lo`` wildcards, used together with a count, can make a
listing:

.. code-block:: text

  GET http://s3p.io/raw//hi2/mbox  -200->  http://s3p.io/raw/2010-06/mbox
                                           http://s3p.io/raw/2010-07/mbox

  GET http://s3p.io/raw//lo2/mbox  -200->  http://s3p.io/raw/2010-04/mbox
                                           http://s3p.io/raw/2010-05/mbox

Counts are the natural numbers starting at 0. The wildcard ``/*`` refers to
"all the items" (``/hi*`` and ``/lo*`` are equivalent so just ``/*`` is
enough.)

A counted wildcard, like ``/hi2``, can be suffixed with a tilde to form it's
complement -- so ``/hi2~`` is everything but the highest two items. This can
be useful for bulk deletion of old/new things.

Examples
--------

.. code-block:: bash

  # Start web application.
  s3p ...

Bugs
----

Unwritten programs are bug free.

