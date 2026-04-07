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
#include "ores.qt/BadgeCache.hpp"

#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

BadgeCache::BadgeCache(ClientManager* clientManager, QObject* parent)
    : QObject(parent),
      clientManager_(clientManager),
      definitions_watcher_(new QFutureWatcher<DefinitionsResult>(this)),
      mappings_watcher_(new QFutureWatcher<MappingsResult>(this)) {

    connect(definitions_watcher_, &QFutureWatcher<DefinitionsResult>::finished,
            this, &BadgeCache::onDefinitionsLoaded);
    connect(mappings_watcher_, &QFutureWatcher<MappingsResult>::finished,
            this, &BadgeCache::onMappingsLoaded);

    BOOST_LOG_SEV(lg(), debug) << "BadgeCache created";
}

void BadgeCache::loadAll() {
    if (is_loading_) {
        BOOST_LOG_SEV(lg(), debug) << "Already loading badge cache, skipping";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading badge definitions and mappings";
    is_loading_ = true;
    definitions_loaded_ = false;
    mappings_loaded_ = false;

    loadDefinitions();
    loadMappings();
}

void BadgeCache::loadDefinitions() {
    QPointer<BadgeCache> self = this;

    QFuture<DefinitionsResult> future = QtConcurrent::run([self]() -> DefinitionsResult {
        if (!self || !self->clientManager_)
            return {false, {}};

        auto result = self->clientManager_->process_authenticated_request(
            dq::messaging::get_badge_definitions_request{});
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch badge definitions: "
                                       << result.error();
            return {false, {}};
        }

        BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->definitions.size()
                                   << " badge definitions";
        return {true, std::move(result->definitions)};
    });

    definitions_watcher_->setFuture(future);
}

void BadgeCache::loadMappings() {
    QPointer<BadgeCache> self = this;

    QFuture<MappingsResult> future = QtConcurrent::run([self]() -> MappingsResult {
        if (!self || !self->clientManager_)
            return {false, {}};

        auto result = self->clientManager_->process_authenticated_request(
            dq::messaging::get_badge_mappings_request{});
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch badge mappings: "
                                       << result.error();
            return {false, {}};
        }

        BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->mappings.size()
                                   << " badge mappings";
        return {true, std::move(result->mappings)};
    });

    mappings_watcher_->setFuture(future);
}

void BadgeCache::onDefinitionsLoaded() {
    auto result = definitions_watcher_->result();
    if (!result.success) {
        is_loading_ = false;
        emit loadError("Failed to fetch badge definitions");
        return;
    }

    definitions_ = std::move(result.definitions);

    definition_index_.clear();
    for (std::size_t i = 0; i < definitions_.size(); ++i)
        definition_index_[definitions_[i].code] = i;

    BOOST_LOG_SEV(lg(), info) << "Cached " << definitions_.size() << " badge definitions";

    definitions_loaded_ = true;
    if (mappings_loaded_) {
        buildIndex();
        is_loading_ = false;
        is_loaded_ = true;
        emit loaded();
    }
}

void BadgeCache::onMappingsLoaded() {
    auto result = mappings_watcher_->result();
    if (!result.success) {
        is_loading_ = false;
        emit loadError("Failed to fetch badge mappings");
        return;
    }

    mappings_ = std::move(result.mappings);
    BOOST_LOG_SEV(lg(), info) << "Cached " << mappings_.size() << " badge mappings";

    mappings_loaded_ = true;
    if (definitions_loaded_) {
        buildIndex();
        is_loading_ = false;
        is_loaded_ = true;
        emit loaded();
    }
}

void BadgeCache::populate_for_testing(
    std::vector<dq::domain::badge_definition> definitions,
    std::vector<dq::messaging::badge_mapping> mappings) {

    definitions_ = std::move(definitions);
    mappings_ = std::move(mappings);

    definition_index_.clear();
    for (std::size_t i = 0; i < definitions_.size(); ++i)
        definition_index_[definitions_[i].code] = i;

    buildIndex();
    is_loaded_ = true;
}

void BadgeCache::buildIndex() {
    index_.clear();
    for (const auto& m : mappings_) {
        auto key = m.code_domain_code + '\0' + m.entity_code;
        auto it = definition_index_.find(m.badge_code);
        if (it != definition_index_.end())
            index_[key] = it->second;
    }
    BOOST_LOG_SEV(lg(), debug) << "Built badge index with " << index_.size() << " entries";
}

const dq::domain::badge_definition* BadgeCache::resolve(
    const std::string& code_domain_code,
    const std::string& entity_code) const {

    if (!is_loaded_)
        return nullptr;

    auto key = code_domain_code + '\0' + entity_code;
    auto it = index_.find(key);
    if (it == index_.end())
        return nullptr;

    return &definitions_[it->second];
}

}
