# DNS Service Discovery for LispWorks
### Version 1.0.1

## Overview

This library is a Common Lisp wrapper around the DNS-SD API.

DNS Service Discovery is also known as [Zeroconf](http://en.wikipedia.org/wiki/Zero-configuration_networking).  It is a way of using standard DNS programming interfaces, servers, and packet formats to browse the network for services.

For an introduction to DNS Service Discovery, see [Apple's documentation](https://developer.apple.com/library/mac/#documentation/Networking/Conceptual/dns_discovery_api/Introduction.html).

## Features

Using the DNS Service Discovery you will be able to:

  -  Register, Browse and Resolve DNS-SD services,
  -  Get IP addresses from browsed hostnames,
  -  Create, query, update and remove DNS resource records,
  -  Create NAT port mappings, or get the NAT's external IP address.

The library has been tested under the following implementations:

  -  LispWorks Professional 32 bits for Mac OS X,
  -  LispWorks Professional 32 bits for Windows,
  -  LispWorks Enterprise 64 bits for Mac OS X.

Testing on other platforms, including Linux, would be welcome.

## Feedback and suggestions

Please contact: Camille Troillard <camille@osculator.net>

