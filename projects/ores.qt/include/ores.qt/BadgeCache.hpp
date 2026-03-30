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
#ifndef ORES_QT_BADGE_CACHE_HPP
#define ORES_QT_BADGE_CACHE_HPP

#include <string>
#include <unordered_map>
#include <vector>
#include <QObject>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/badge_definition.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"

namespace ores::qt {

/**
 * @brief Client-side cache of badge definitions and mappings.
 *
 * Loaded at startup after login. Provides fast synchronous lookup of a badge
 * definition for any (code_domain_code, entity_code) pair.
 *
 * Typical usage:
 * 1. Create once in MainWindow, call loadAll() after loggedIn signal.
 * 2. Pass to any widget that needs to render badges.
 * 3. Call resolve() at render time — always returns immediately.
 */
class BadgeCache final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.badge_cache";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BadgeCache(ClientManager* clientManager, QObject* parent = nullptr);
    ~BadgeCache() override = default;

    /**
     * @brief Load badge definitions and mappings from the server.
     *
     * Should be called after successful login. Safe to call multiple times;
     * a load already in progress is a no-op.
     */
    void loadAll();

    /**
     * @brief Returns true once both definitions and mappings are loaded.
     */
    bool isLoaded() const { return is_loaded_; }

    /**
     * @brief Inject test data directly, bypassing the network load path.
     *
     * Sets definitions and mappings, builds the lookup index, and marks the
     * cache as loaded. Intended for unit tests only.
     */
    void populate_for_testing(
        std::vector<dq::domain::badge_definition> definitions,
        std::vector<dq::messaging::badge_mapping> mappings);

    /**
     * @brief Resolve a badge definition for a given (code_domain, entity_code) pair.
     *
     * @param code_domain_code The code domain (e.g., "party_status").
     * @param entity_code      The entity value (e.g., "ACTIVE").
     * @return Pointer to the badge_definition if a mapping exists, nullptr otherwise.
     *
     * Always returns immediately without blocking. Returns nullptr if the cache
     * is not yet loaded or no mapping exists for the given pair.
     */
    const dq::domain::badge_definition* resolve(
        const std::string& code_domain_code,
        const std::string& entity_code) const;

signals:
    /**
     * @brief Emitted when both definitions and mappings are loaded.
     */
    void loaded();

    /**
     * @brief Emitted when an error occurs during loading.
     */
    void loadError(const QString& error_message);

private slots:
    void onDefinitionsLoaded();
    void onMappingsLoaded();

private:
    void loadDefinitions();
    void loadMappings();
    void buildIndex();

    struct DefinitionsResult {
        bool success;
        std::vector<dq::domain::badge_definition> definitions;
    };

    struct MappingsResult {
        bool success;
        std::vector<dq::messaging::badge_mapping> mappings;
    };

    ClientManager* clientManager_;

    std::vector<dq::domain::badge_definition> definitions_;
    std::vector<dq::messaging::badge_mapping> mappings_;

    // (code_domain_code + '\0' + entity_code) -> index into definitions_
    std::unordered_map<std::string, std::size_t> index_;

    // badge_code -> index into definitions_
    std::unordered_map<std::string, std::size_t> definition_index_;

    bool is_loaded_{false};
    bool is_loading_{false};
    bool definitions_loaded_{false};
    bool mappings_loaded_{false};

    QFutureWatcher<DefinitionsResult>* definitions_watcher_;
    QFutureWatcher<MappingsResult>* mappings_watcher_;
};

}

#endif
