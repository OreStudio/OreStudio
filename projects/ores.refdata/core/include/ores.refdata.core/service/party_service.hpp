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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing parties.
 *
 * Provides a higher-level interface for party operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_service(context ctx);

    /**
     * @brief Lists parties with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of parties for the requested page.
     */
    std::vector<domain::party> list_parties(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active parties.
     *
     * @return Total number of active parties.
     */
    std::uint32_t count_parties();

    /**
     * @brief Retrieves a single party as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the party.
     * @param version The version to fetch.
     * @return The party at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party> get_party_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single party by its id.
     *
     * @param id The id of the party.
     * @return The party if found, std::nullopt otherwise.
     */
    std::optional<domain::party> get_party(const std::string& id);

    /**
     * @brief Saves a party (creates or updates).
     *
     * @param party The party to save.
     * @throws std::exception on failure.
     */
    void save_party(const domain::party& party);

    /**
     * @brief Saves a batch of parties.
     *
     * @param parties The parties to save.
     * @throws std::exception on failure.
     */
    void save_parties(const std::vector<domain::party>& parties);

    /**
     * @brief Deletes a party by its id.
     *
     * @param id The id of the party to delete.
     * @throws std::exception on failure.
     */
    void delete_party(const std::string& id);

    /**
     * @brief Deletes parties by their ids.
     */
    void delete_parties(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party.
     */
    std::vector<domain::party> get_party_history(const std::string& id);

    /**
     * @brief Gets the party hierarchy (as a forest of trees) rooted
     * at, or containing, the given party.
     *
     * @param root_id The party to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::party_repository repo_;
};

}

#endif
