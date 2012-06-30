======================================================
 s3dist - software distribution backed by S3
======================================================

Synopsis
--------

.. code-block:: text

    s3dist ...

Description
-----------

S3Dist is a proxy for S3 that can generate short-lived, signed URLs for stored
objects. By providing a server separate from S3 that can be placed behind an
authenticating proxy or firewall, S3Dist allows a variety of common security
mechanisms to be used to limit access to S3 objects over HTTP while taking
advantage of S3's considerable bandwidth and parallelism.

Use-cases for S3Dist include:

  * sharing of large files within an organization,

  * media service for public facing web applications,

  * distribution of proprietary code and data for automated deployments.

Options
-------

  ``-lolno``
    No LOLs are permitted with this option.

REST Interface
--------------

.. code-block:: text

  http://s3.dist/a/path
    GET     ->  303 to S3 signed URL
    PUT     ->  Object is stored.
    DELETE  ->  Object is removed.

Examples
--------

.. code-block:: bash

  # Start web application.
  s3dist ...

Bugs
----

Unwritten programs are bug free.

