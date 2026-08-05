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
#include "ores.dq.core/service/lei_entity_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

lei_entity_service::lei_entity_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::lei_entity> lei_entity_service::list_entities(std::uint32_t offset,
                                                                  std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all LEI entities";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lei_entity_service::count_entities() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total LEI entities count";
    return repo_.get_total_entity_count(ctx_);
}


std::optional<domain::lei_entity> lei_entity_service::get_entity_at_version(const std::string& lei,
                                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI entity at version. " << "lei: " << lei
                               << " version: " << version;
    return repo_.read_at_version(ctx_, lei, version);
}

std::optional<domain::lei_entity> lei_entity_service::get_entity(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI entity. " << "lei: " << lei;
    auto results = repo_.read_latest(ctx_, lei);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void lei_entity_service::save_entity(const domain::lei_entity& v) {
    if (v.lei.empty())
        throw std::invalid_argument("LEI Entity lei cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving LEI entity. " << "lei: " << v.lei;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved LEI entity. " << "lei: " << v.lei;
}

void lei_entity_service::save_entities(const std::vector<domain::lei_entity>& entities) {
    for (const auto& e : entities) {
        if (e.lei.empty())
            throw std::invalid_argument("LEI Entity lei cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << entities.size() << " LEI entities";
    auto ts = entities;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void lei_entity_service::delete_entity(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI entity. " << "lei: " << lei;
    repo_.remove(ctx_, lei);
    BOOST_LOG_SEV(lg(), info) << "Removed LEI entity. " << "lei: " << lei;
}

void lei_entity_service::delete_entities(const std::vector<std::string>& leis) {
    repo_.remove(ctx_, leis);
}

std::vector<domain::lei_entity> lei_entity_service::get_entity_history(const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for LEI entity. " << "lei: " << lei;
    return repo_.read_all(ctx_, lei);
}

}
