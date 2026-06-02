#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# Generates SSL certificates for the HTTP server and an RSA-2048 private key
# for IAM JWT signing. Existing files are skipped; delete them to regenerate.
#
# Run this from the publish/bin directory after building the project.
#
# Usage:
#   ./generate_certs.sh

set -e

# SSL Certificates
if [[ -f server.key && -f server.crt ]]; then
    echo "SSL certificates already exist (server.key, server.crt) - skipping."
else
    echo "Generating SSL certificates..."
    openssl genrsa -out server.key 2048
    openssl req -new -key server.key -out server.csr \
        -subj "/C=US/ST=State/L=City/O=Organization/OU=OrgUnit/CN=localhost"
    openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
    rm server.csr
    echo "Generated server.key and server.crt"
fi

# IAM JWT Signing Key
if [[ -f iam-rsa-private.pem ]]; then
    echo "IAM RSA key already exists (iam-rsa-private.pem) - skipping."
else
    echo "Generating IAM RSA-2048 private key for JWT signing..."
    openssl genrsa -out iam-rsa-private.pem 2048
    chmod 600 iam-rsa-private.pem
    echo "Generated iam-rsa-private.pem"
fi

echo ""
echo "Done. Reload ores-prodigy.el and restart services to pick up new files."
