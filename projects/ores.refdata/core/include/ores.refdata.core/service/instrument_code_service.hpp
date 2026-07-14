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
#ifndef ORES_REFDATA_CORE_SERVICE_INSTRUMENT_CODE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_INSTRUMENT_CODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/instrument_code_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing instrument codes.
 *
 * Provides a higher-level interface for instrument code operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT instrument_code_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.instrument_code_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_code_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_code_service(context ctx);

    /**
     * @brief Lists instrument codes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument codes for the requested page.
     */
    std::vector<domain::instrument_code> list_instruments(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument codes.
     *
     * @return Total number of active instrument codes.
     */
    std::uint32_t count_instruments();

    /**
     * @brief Retrieves a single instrument code as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the instrument code.
     * @param version The version to fetch.
     * @return The instrument code at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_code> get_instrument_at_version(const std::string& code,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single instrument code by its code.
     *
     * @param code The code of the instrument code.
     * @return The instrument code if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_code> get_instrument(const std::string& code);

    /**
     * @brief Saves a instrument code (creates or updates).
     *
     * @param instrument The instrument code to save.
     * @throws std::exception on failure.
     */
    void save_instrument(const domain::instrument_code& instrument);

    /**
     * @brief Saves a batch of instrument codes.
     *
     * @param instruments The instrument codes to save.
     * @throws std::exception on failure.
     */
    void save_instruments(const std::vector<domain::instrument_code>& instruments);

    /**
     * @brief Deletes a instrument code by its code.
     *
     * @param code The code of the instrument code to delete.
     * @throws std::exception on failure.
     */
    void delete_instrument(const std::string& code);

    /**
     * @brief Deletes instrument codes by their codes.
     */
    void delete_instruments(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a instrument code.
     */
    std::vector<domain::instrument_code> get_instrument_history(const std::string& code);

private:
    context ctx_;
    repository::instrument_code_repository repo_;
};

}

#endif
