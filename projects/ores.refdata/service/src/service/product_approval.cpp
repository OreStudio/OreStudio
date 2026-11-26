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
#include "ores.refdata.service/service/product_approval.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <span>
#include <string>

namespace ores::refdata::service {

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.refdata.service.product_approval");
    return instance;
}

constexpr auto query = R"(select book_id::text,
       permits_every_type::text,
       product_type_code
from ores_refdata_book_product_approvals_fn(string_to_array($1, ',')::uuid[]))";

}

std::vector<ores::refdata::messaging::book_product_approval>
book_product_approvals_from(std::span<const product_approval_row> rows) {
    std::vector<ores::refdata::messaging::book_product_approval> approvals;
    const auto uuid_of = [](const std::string& text) {
        return boost::uuids::string_generator{}(text);
    };
    /*
     * The rows come in the order of the books asked for, and within a book in the
     * order of its types, so a book's answer opens on its first row and every row
     * that names it after that adds a type.
     */
    std::map<std::string, std::size_t> index_of;
    for (const auto& row : rows) {
        if (row.book_id.empty())
            continue;
        auto entry = index_of.find(row.book_id);
        if (entry == index_of.end()) {
            entry = index_of.emplace(row.book_id, approvals.size()).first;
            approvals.push_back(ores::refdata::messaging::book_product_approval{
                .book_id = uuid_of(row.book_id),
                .allowed_product_types = std::vector<std::string>{}});
        }
        auto& approval = approvals[entry->second];
        if (row.permits_every_type) {
            approval.allowed_product_types = std::nullopt;
            continue;
        }
        if (!row.product_type_code.empty())
            approval.allowed_product_types->push_back(row.product_type_code);
    }
    return approvals;
}

std::vector<ores::refdata::messaging::book_product_approval>
book_product_approvals(const ores::database::context& ctx,
                       const std::vector<boost::uuids::uuid>& book_ids) {
    if (book_ids.empty())
        return {};

    std::string requested;
    for (const auto& book_id : book_ids) {
        if (!requested.empty())
            requested += ',';
        requested += boost::uuids::to_string(book_id);
    }

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx, query, {requested}, lg(), "Reading the product types each book permits.");

    std::vector<product_approval_row> read;
    read.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0].has_value())
            continue;
        read.push_back(product_approval_row{
            .book_id = *row[0],
            .permits_every_type = row[1].has_value() && *row[1] == "true",
            .product_type_code = row[2].has_value() ? *row[2] : std::string{}});
    }
    return book_product_approvals_from(read);
}

}
