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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_REQUEST_STORY_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_REQUEST_STORY_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http/routes/workflow/story_events.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace ores::http::routes::workflow {

/**
 * @brief One request's whole story, newest first.
 *
 * The request's own versions, the answers given on it, the roles it asked for and
 * what became of them, and the notices this reader was given, each a read of its
 * own. The parts are not equally visible, and that is the design: the request and
 * its roles are the asker's to read, and the decisions another person took answer
 * only to a reader who may read decisions, so a member's story has a hole exactly
 * where the answer was given.
 *
 * The composition is shared with the timeline, which wraps it rather than reading
 * the request again, so the two cannot drift into two shapes.
 */
[[nodiscard]] std::vector<story_event>
request_story_events(const ores::http::domain::http_request& req,
                     ores::nats::service::nats_client& session,
                     const std::string& request_id);

}

#endif
