/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
set schema 'metadata';

drop function if exists metadata.dq_list_populatable_datasets_fn;
drop function if exists metadata.dq_image_preview_fn;
drop function if exists metadata.dq_images_publish_fn;
drop function if exists metadata.dq_country_preview_fn;
drop function if exists metadata.dq_countries_publish_fn;
drop function if exists metadata.dq_currency_preview_fn;
drop function if exists metadata.dq_currencies_publish_fn;
drop function if exists metadata.dq_preview_ip2country_population_fn;
drop function if exists metadata.dq_ip2country_publish_fn;
