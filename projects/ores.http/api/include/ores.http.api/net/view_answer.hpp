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
#ifndef ORES_HTTP_API_NET_VIEW_ANSWER_HPP
#define ORES_HTTP_API_NET_VIEW_ANSWER_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/export.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::net {

/**
 * @brief How long a composed view's answer stays fresh in the caller's own
 * cache, or nothing for a view computed again at each read.
 *
 * A view is cached only by the caller, because each caller's session decides
 * what the view holds.
 */
struct view_cache_policy final {
    std::optional<std::chrono::seconds> stale_after;
};

/**
 * @brief The entity tag of a body: a strong tag over its bytes, so two answers
 * share a tag when they share their bytes.
 *
 * The tag is a 64-bit hash and the length, enough to tell versions of an answer
 * apart. It is not a security measure: nothing trusts it beyond saving a body.
 */
ORES_HTTP_API_EXPORT std::string entity_tag(std::string_view body);

/**
 * @brief A composed view's JSON answer under its cache policy.
 *
 * A view with a freshness rule carries its entity tag and stays fresh in the
 * caller's private cache for that long, varying by the caller's credential; a
 * request whose If-None-Match names the tag answers 304 with no body. A view
 * with no freshness rule is not stored.
 */
ORES_HTTP_API_EXPORT domain::http_response
view_answer(const domain::http_request& req, std::string body, const view_cache_policy& policy);

}

#endif
