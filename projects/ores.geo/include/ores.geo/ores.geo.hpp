/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_GEO_HPP
#define ORES_GEO_HPP

/**
 * @brief Geolocation services module for ORE Studio.
 *
 * This module provides IP-based geolocation capabilities using MaxMind GeoIP2
 * databases. Key features include:
 *
 * - IP to location: resolve IP addresses to geographic information
 * - Country detection: identify the country associated with an IP address
 * - ASN lookup: determine the autonomous system number for network analysis
 *
 * The module is organized into namespaces: service (geolocation service
 * implementation).
 */
namespace ores::geo {}

#endif
