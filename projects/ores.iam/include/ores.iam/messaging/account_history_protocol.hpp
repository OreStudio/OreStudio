/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_HISTORY_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_HISTORY_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.iam/domain/account_version.hpp"

namespace ores::iam::messaging {

struct account_version_history {
    std::vector<ores::iam::domain::account_version> versions;
};

struct get_account_history_request {
    std::string username;
};

struct get_account_history_response {
    bool success = false;
    std::string message;
    account_version_history history;
};

}

#endif
