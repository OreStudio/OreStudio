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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_CLIENT_HPP
#define ORES_IAM_CLIENT_HPP

/**
 * @brief Client-side helpers for IAM operations.
 *
 * This namespace provides high-level helper functions for client applications
 * to perform authentication operations. These helpers encapsulate the
 * request/response protocol and session state management, eliminating
 * duplicated code across different client implementations (shell, Qt, etc.).
 */
namespace ores::iam::client {}

#include "ores.iam/client/auth_helpers.hpp"

#endif
