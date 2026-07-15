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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_CONTACT_INFORMATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_CONTACT_INFORMATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_contact_information_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty contact informations.
 *
 * Provides a higher-level interface for counterparty contact information operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_contact_information_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_contact_information_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_contact_information_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_contact_information_service(context ctx);

    /**
     * @brief Lists counterparty contact informations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty contact informations for the requested page.
     */
    std::vector<domain::counterparty_contact_information>
    list_counterparty_contact_informations(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty contact informations.
     *
     * @return Total number of active counterparty contact informations.
     */
    std::uint32_t count_counterparty_contact_informations();

    /**
     * @brief Lists counterparty contact informations filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching counterparty contact informations for the requested page.
     */
    std::vector<domain::counterparty_contact_information>
    list_counterparty_contact_informations_by_counterparty_id(const std::string& counterparty_id,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty contact informations filtered by
     * counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching counterparty contact informations.
     */
    std::uint32_t
    count_counterparty_contact_informations_by_counterparty_id(const std::string& counterparty_id);

    /**
     * @brief Lists counterparty contact informations filtered by counterparty_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching counterparty contact informations.
     */
    std::vector<domain::counterparty_contact_information>
    list_counterparty_contact_informations_by_counterparty_id_as_of(
        const std::string& counterparty_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Retrieves a single counterparty contact information as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the counterparty contact information.
     * @param version The version to fetch.
     * @return The counterparty contact information at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::counterparty_contact_information>
    get_counterparty_contact_information_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty contact information by its id.
     *
     * @param id The id of the counterparty contact information.
     * @return The counterparty contact information if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_contact_information>
    get_counterparty_contact_information(const std::string& id);

    /**
     * @brief Saves a counterparty contact information (creates or updates).
     *
     * @param counterparty_contact_information The counterparty contact information to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_contact_information(
        const domain::counterparty_contact_information& counterparty_contact_information);

    /**
     * @brief Saves a batch of counterparty contact informations.
     *
     * @param counterparty_contact_informations The counterparty contact informations to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_contact_informations(
        const std::vector<domain::counterparty_contact_information>&
            counterparty_contact_informations);

    /**
     * @brief Deletes a counterparty contact information by its id.
     *
     * @param id The id of the counterparty contact information to delete.
     * @throws std::exception on failure.
     */
    void delete_counterparty_contact_information(const std::string& id);

    /**
     * @brief Deletes counterparty contact informations by their ids.
     */
    void delete_counterparty_contact_informations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty contact information.
     */
    std::vector<domain::counterparty_contact_information>
    get_counterparty_contact_information_history(const std::string& id);

private:
    context ctx_;
    repository::counterparty_contact_information_repository repo_;
};

}

#endif
