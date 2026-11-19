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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_TIMELINE_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_TIMELINE_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/workflow/story_events.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::workflow {

/**
 * @brief A source the stream could not draw from, and why.
 *
 * A gap is part of the answer rather than a failure of it. A source this caller
 * may not read, a record whose history the service does not hold, a window that
 * has aged out: each is a fact about what the product can say, and a reader who
 * is not told it will read the stream as complete.
 */
struct timeline_gap final {
    std::string entity;
    std::string reason;
};

/// One subject's story: what happened, and what the stream cannot show.
struct timeline_answer final {
    std::string subject;
    std::string id;
    std::vector<story_event> events;
    std::vector<timeline_gap> gaps;
};

/**
 * @brief One person's whole story, newest first.
 *
 * The account, its contact record, the roles it holds, the parties it works in
 * and its sign-ins are five reads over five tables, merged into one stream. The
 * parts are not equally answerable, and the stream says so rather than rounding:
 * the sign-in log keeps a window rather than everything, and a source this
 * caller may not read is missing. Each is a gap on the answer.
 *
 * A username nothing holds answers with an empty stream, which is what the
 * person screen answers for the same name.
 */
[[nodiscard]] timeline_answer person_timeline(const ores::http::domain::http_request& req,
                                              ores::nats::service::nats_client& session,
                                              const std::string& username);

/**
 * @brief One party's whole story, newest first.
 *
 * A party is made of several kinds of record, so its story is the party's own
 * versions merged with those of its identifiers, its contacts and its business
 * units, and with the opening and closing of the countries and currencies it
 * works in and the counterparties it may trade with. A link has two keys, so it
 * is read by the party and answers every link the party ever held.
 *
 * A party nothing holds answers with an empty stream, as the party page does.
 */
[[nodiscard]] timeline_answer party_timeline(const ores::http::domain::http_request& req,
                                             ores::nats::service::nats_client& session,
                                             const std::string& party_id);

/// The answer, as the browser reads it.
[[nodiscard]] std::string timeline_json(const timeline_answer& answer);

/**
 * @brief One subject's story in one stream, at the BFF's own address.
 *
 * The browser names a subject, never an entity type. Which rows make up a
 * person's story is the server's business, and so is which of them this caller
 * may read: a source that refuses becomes a gap on the answer rather than a hole
 * a screen cannot explain.
 */
class timeline_routes final {
public:
    /// The timeline, kept fresh for thirty seconds.
    static ores::http::domain::http_response
    view_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
