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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_PAGE_QUERY_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_PAGE_QUERY_HPP

#include "ores.http.api/domain/http_request.hpp"
#include <cstdint>
#include <string_view>

namespace ores::http::routes::workflow {

/**
 * @brief How many rows a read skips and how many it answers with.
 *
 * Every list in this part is paged the same way, and each states its own bound
 * and its own sentence, because a page of requests costs a join per row and a
 * page of notices does not.
 */
struct page final {
    std::uint32_t offset = 0;
    std::uint32_t limit = 0;
};

/**
 * @brief The page a read states, refused by the sentence when it states one the
 * read does not take.
 *
 * The read takes =offset= and =limit= and nothing else: a query naming another
 * parameter is refused rather than answered with it silently dropped. The offset
 * may be any whole number, the limit is from one to @p page_max, and a read that
 * states neither answers @p page_default rows.
 */
page page_of(const ores::http::domain::http_request& req,
             std::uint32_t page_default,
             std::uint32_t page_max,
             std::string_view refused);

}

#endif
