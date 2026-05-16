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
#ifndef ORES_CONNECTIONS_REPOSITORY_RECENT_PARTY_REPOSITORY_HPP
#define ORES_CONNECTIONS_REPOSITORY_RECENT_PARTY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/recent_party.hpp"
#include "ores.connections/repository/sqlite_context.hpp"
#include "ores.connections/export.hpp"

namespace ores::connections::repository {

class ORES_CONNECTIONS_EXPORT recent_party_repository final {
public:
    static constexpr int max_recents = 5;

    explicit recent_party_repository(sqlite_context& ctx);

    /**
     * @brief Upsert a party selection and prune to max_recents.
     */
    void record(const boost::uuids::uuid& party_id, const std::string& party_name);

    /**
     * @brief Return up to max_recents parties, most-recently-selected first.
     */
    std::vector<domain::recent_party> read_recent();

private:
    sqlite_context& ctx_;
};

}

#endif
