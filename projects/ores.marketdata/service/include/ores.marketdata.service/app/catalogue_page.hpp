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
#ifndef ORES_MARKETDATA_SERVICE_APP_CATALOGUE_PAGE_HPP
#define ORES_MARKETDATA_SERVICE_APP_CATALOGUE_PAGE_HPP

#include <cstdint>
#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief How many rows one page of a filtered catalogue read asks for.
 *
 * A request names one curve config, one surface blueprint or one pair, and the
 * rows behind it are tens. One page covers them, and the loop in
 * read_all_pages() is what makes a tenant that outgrows the page cost a second
 * read rather than answer with a part of its catalogue.
 */
inline constexpr std::uint32_t catalogue_page_size = 500;

/**
 * @brief Reads every page of one filtered catalogue read.
 *
 * The read names the rows it wants in the query, and this walks the pages of
 * the answer, so the caller's cost follows the rows it named and not the rows
 * the table holds.
 *
 * @param read_page Takes an offset and a limit, and answers that page.
 */
template <typename ReadPage>
[[nodiscard]] auto read_all_pages(ReadPage&& read_page) {
    using page_type = std::invoke_result_t<ReadPage, std::uint32_t, std::uint32_t>;
    using row_type = typename page_type::value_type;

    std::vector<row_type> rows;
    for (std::uint32_t offset = 0;; offset += catalogue_page_size) {
        auto page = read_page(offset, catalogue_page_size);
        rows.insert(
            rows.end(), std::make_move_iterator(page.begin()), std::make_move_iterator(page.end()));
        if (page.size() < catalogue_page_size)
            break;
    }
    return rows;
}

}

#endif
