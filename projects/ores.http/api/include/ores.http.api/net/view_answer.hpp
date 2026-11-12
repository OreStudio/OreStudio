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
#include <string>
#include <string_view>

namespace ores::http::net {

/**
 * @brief How a composed view's answer is kept by the caller.
 *
 * A view is kept only by the caller, because each caller's session decides what
 * the view holds. A view the caller's own writes change is revalidated at each
 * read, so a screen never shows the view from before its own write; only a view
 * no write of the caller changes stays fresh for a while.
 */
class view_cache_policy final {
public:
    /// Not kept: computed and sent whole at each read.
    static view_cache_policy not_stored() {
        return view_cache_policy(false, std::chrono::seconds(0));
    }

    /// Kept, and asked about at each read: an unchanged view answers 304.
    static view_cache_policy revalidated() {
        return view_cache_policy(true, std::chrono::seconds(0));
    }

    /// Kept, and used without asking for this long; then as revalidated.
    static view_cache_policy fresh_for(std::chrono::seconds stale_after) {
        return view_cache_policy(true, stale_after);
    }

    [[nodiscard]] bool kept() const {
        return kept_;
    }

    [[nodiscard]] std::chrono::seconds stale_after() const {
        return stale_after_;
    }

private:
    view_cache_policy(bool kept, std::chrono::seconds stale_after)
        : kept_(kept)
        , stale_after_(stale_after) {}

    bool kept_;
    std::chrono::seconds stale_after_;
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
 * A kept view carries its entity tag, is kept privately and varies by the
 * caller's credential; a request whose If-None-Match names the tag answers 304
 * with no body. A view that is not kept is sent whole and not stored.
 */
ORES_HTTP_API_EXPORT domain::http_response
view_answer(const domain::http_request& req, std::string body, const view_cache_policy& policy);

}

#endif
