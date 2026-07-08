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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparties.
 *
 * Provides a higher-level interface for counterparty operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.counterparty_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_service(context ctx);

    /**
     * @brief Lists counterparties with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparties for the requested page.
     */
    std::vector<domain::counterparty> list_counterparties(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparties.
     *
     * @return Total number of active counterparties.
     */
    std::uint32_t count_counterparties();

    /**
     * @brief Retrieves a single counterparty as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the counterparty.
     * @param version The version to fetch.
     * @return The counterparty at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> get_counterparty_at_version(const std::string& id,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty by its id.
     *
     * @param id The id of the counterparty.
     * @return The counterparty if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> get_counterparty(const std::string& id);

    /**
     * @brief Saves a counterparty (creates or updates).
     *
     * @param counterparty The counterparty to save.
     * @throws std::exception on failure.
     */
    void save_counterparty(const domain::counterparty& counterparty);

    /**
     * @brief Saves a batch of counterparties.
     *
     * @param counterparties The counterparties to save.
     * @throws std::exception on failure.
     */
    void save_counterparties(const std::vector<domain::counterparty>& counterparties);

    /**
     * @brief Deletes a counterparty by its id.
     *
     * @param id The id of the counterparty to delete.
     * @throws std::exception on failure.
     */
    void delete_counterparty(const std::string& id);

    /**
     * @brief Deletes counterparties by their ids.
     */
    void delete_counterparties(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty.
     */
    std::vector<domain::counterparty> get_counterparty_history(const std::string& id);

    /**
     * @brief Gets the counterparty hierarchy (as a forest of trees) rooted
     * at, or containing, the given counterparty.
     *
     * @param root_id The counterparty to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::counterparty_repository repo_;
};

}

#endif
