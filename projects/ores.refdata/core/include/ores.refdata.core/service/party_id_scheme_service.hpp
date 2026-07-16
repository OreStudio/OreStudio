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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_ID_SCHEME_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_ID_SCHEME_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_id_scheme.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_id_scheme_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party ID schemes.
 *
 * Provides a higher-level interface for party ID scheme operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_id_scheme_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_id_scheme_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_id_scheme_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_id_scheme_service(context ctx);

    /**
     * @brief Lists party ID schemes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party ID schemes for the requested page.
     */
    std::vector<domain::party_id_scheme> list_schemes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party ID schemes.
     *
     * @return Total number of active party ID schemes.
     */
    std::uint32_t count_schemes();

    /**
     * @brief Retrieves a single party ID scheme as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the party ID scheme.
     * @param version The version to fetch.
     * @return The party ID scheme at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_id_scheme> get_scheme_at_version(const std::string& code,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single party ID scheme by its code.
     *
     * @param code The code of the party ID scheme.
     * @return The party ID scheme if found, std::nullopt otherwise.
     */
    std::optional<domain::party_id_scheme> get_scheme(const std::string& code);

    /**
     * @brief Saves a party ID scheme (creates or updates).
     *
     * @param scheme The party ID scheme to save.
     * @throws std::exception on failure.
     */
    void save_scheme(const domain::party_id_scheme& scheme);

    /**
     * @brief Saves a batch of party ID schemes.
     *
     * @param schemes The party ID schemes to save.
     * @throws std::exception on failure.
     */
    void save_schemes(const std::vector<domain::party_id_scheme>& schemes);

    /**
     * @brief Deletes a party ID scheme by its code.
     *
     * @param code The code of the party ID scheme to delete.
     * @throws std::exception on failure.
     */
    void delete_scheme(const std::string& code);

    /**
     * @brief Deletes party ID schemes by their codes.
     */
    void delete_schemes(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a party ID scheme.
     */
    std::vector<domain::party_id_scheme> get_scheme_history(const std::string& code);

private:
    context ctx_;
    repository::party_id_scheme_repository repo_;
};

}

#endif
