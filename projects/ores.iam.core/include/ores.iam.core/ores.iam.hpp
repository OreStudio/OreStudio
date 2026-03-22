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
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_HPP
#define ORES_IAM_HPP

/**
 * @brief Identity and Access Management (IAM) module for ORE Studio.
 *
 * This module provides comprehensive user account management with secure
 * authentication and authorization. Key features include:
 *
 * - Account lifecycle: creation, listing, and deletion of user accounts
 * - Authentication: secure password hashing using scrypt, login tracking
 * - Security: account locking after failed login attempts, unlock capability
 * - Message-based API: asynchronous request/response handlers for client-server
 *   communication (subsystem range 0x2000-0x2FFF)
 * - Temporal database support: version tracking with valid_from/valid_to fields
 * - Synthetic data generation: test account generation for development
 *
 * The module is organized into namespaces: domain (core entities), repository
 * (ORM and persistence), service (business logic), messaging (API handlers),
 * security (password management), and generators (test data).
 */
namespace ores::iam {}

#endif
