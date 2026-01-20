/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/publication_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/repository/publication_entity.hpp"
#include "ores.dq/repository/publication_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

publication_repository::publication_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::publication>
publication_repository::read_by_dataset(const boost::uuids::uuid& dataset_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading publications for dataset: "
                               << dataset_id;

    const auto dataset_id_str = boost::uuids::to_string(dataset_id);
    const auto query = sqlgen::read<std::vector<publication_entity>> |
        where("dataset_id"_c == dataset_id_str) |
        order_by("published_at"_c.desc());

    return execute_read_query<publication_entity, domain::publication>(
        ctx_, query,
        [](const auto& entities) { return publication_mapper::map(entities); },
        lg(), "Reading publications by dataset");
}

std::vector<domain::publication>
publication_repository::read_recent(std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading recent publications, limit: " << limit;

    const auto query = sqlgen::read<std::vector<publication_entity>> |
        order_by("published_at"_c.desc()) |
        sqlgen::limit(static_cast<std::size_t>(limit));

    return execute_read_query<publication_entity, domain::publication>(
        ctx_, query,
        [](const auto& entities) { return publication_mapper::map(entities); },
        lg(), "Reading recent publications");
}

void publication_repository::insert(const domain::publication& pub) {
    BOOST_LOG_SEV(lg(), debug) << "Inserting publication record for dataset: "
                               << pub.dataset_code;

    auto entity = publication_mapper::to_entity(pub);

    const auto query = sqlgen::insert(entity);
    const auto result = sqlgen::session(ctx_.connection_pool()).and_then(query);

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to insert publication record: "
                                   << result.error().what();
        throw std::runtime_error("Failed to insert publication record");
    }

    BOOST_LOG_SEV(lg(), info) << "Inserted publication record for dataset: "
                              << pub.dataset_code;
}

}
