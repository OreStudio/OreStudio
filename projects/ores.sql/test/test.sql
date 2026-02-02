/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * pgTAP extension setup.
 * Run this once to install the pgTAP extension before running tests.
 *
 * Usage: psql -U postgres -d <database> -f test.sql
 */

create extension if not exists pgtap;
