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
#ifndef ORES_REFDATA_MESSAGING_PARTY_ID_SCHEME_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PARTY_ID_SCHEME_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.refdata/domain/party_id_scheme.hpp"

namespace ores::refdata::messaging {

struct get_party_id_schemes_request {
    using response_type = struct get_party_id_schemes_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.party-id-schemes.list";
    int offset = 0;
    int limit = 100;
};

struct get_party_id_schemes_response {
    std::vector<ores::refdata::domain::party_id_scheme> party_id_schemes;
    int total_available_count = 0;
};

struct save_party_id_scheme_request {
    using response_type = struct save_party_id_scheme_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.party-id-schemes.save";
    ores::refdata::domain::party_id_scheme data;
};

struct save_party_id_scheme_response {
    bool success = false;
    std::string message;
};

struct delete_party_id_scheme_request {
    using response_type = struct delete_party_id_scheme_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.party-id-schemes.delete";
    std::string scheme;
};

struct delete_party_id_scheme_response {
    bool success = false;
    std::string message;
};

struct get_party_id_scheme_history_request {
    using response_type = struct get_party_id_scheme_history_response;
    static constexpr std::string_view nats_subject = "ores.refdata.v1.party-id-schemes.history";
    std::string scheme;
};

struct get_party_id_scheme_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::refdata::domain::party_id_scheme> history;
};

}

#endif
