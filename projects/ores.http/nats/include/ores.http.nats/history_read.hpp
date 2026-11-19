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
#ifndef ORES_HTTP_NATS_HISTORY_READ_HPP
#define ORES_HTTP_NATS_HISTORY_READ_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.nats/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

/**
 * @brief One record's history, read as the caller.
 *
 * Every component that registers a history provider is read through the same
 * address, and the address names the component in the entity type. The read is
 * therefore one rule, stated once: a route that needs a record's versions does
 * not derive a subject of its own.
 */
namespace ores::http::net::history {

/// One field of one version, as the history layer stamped it.
struct field final {
    std::string name;
    std::string value;
};

/// One field's move between two versions.
struct change final {
    std::string field;
    std::string before;
    std::string after;
};

/// One version of one record.
struct version final {
    std::uint32_t version{};
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::vector<field> fields;
    std::vector<change> changes;
};

/**
 * @brief What one record's history answered.
 *
 * The service states three kinds of bad news in a reply it did send: it holds no
 * read for the entity type, the caller does not hold the permission the read
 * asks for, and the read itself failed. It states all three as one flag and one
 * sentence, so a caller that does not read the service's prose cannot tell "this
 * chapter is missing" from "the deployment is broken", and this step does not
 * read prose.
 *
 * A caller must therefore say only what it knows. An unanswered read is stated as
 * the service not answering, which is true of all three, and never as a
 * permission the person lacks. A refusal the transport carries is a different
 * thing: it is thrown, and a caller that can act on it keeps its status.
 */
struct answer final {
    bool answered = false;
    std::string message;
    std::vector<version> versions;
};

/// Every version of one record, newest first.
[[nodiscard]] ORES_HTTP_NATS_EXPORT answer read(const ores::http::domain::http_request& req,
                                                ores::nats::service::nats_client& session,
                                                std::string_view entity_type,
                                                std::string_view entity_id);

}

#endif
