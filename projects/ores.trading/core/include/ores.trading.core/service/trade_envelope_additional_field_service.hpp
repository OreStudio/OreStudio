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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_ADDITIONAL_FIELD_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_ADDITIONAL_FIELD_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_envelope_additional_field.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade envelope additional fields.
 *
 * Provides a higher-level interface for trade envelope additional field operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_envelope_additional_field_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.trade_envelope_additional_field_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_envelope_additional_field_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_envelope_additional_field_service(context ctx);

    /**
     * @brief Lists trade envelope additional fields with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade envelope additional fields for the requested page.
     */
    std::vector<domain::trade_envelope_additional_field>
    list_trade_envelope_additional_fields(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade envelope additional fields.
     *
     * @return Total number of active trade envelope additional fields.
     */
    std::uint32_t count_trade_envelope_additional_fields();


    /**
     * @brief Retrieves a single trade envelope additional field as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade envelope additional field at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_envelope_additional_field>
    get_trade_envelope_additional_field_at_version(const std::string& trade_id,
                                                   const std::string& sequence_number,
                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single trade envelope additional field by its primary key.
     *
     * @return The trade envelope additional field if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_envelope_additional_field>
    get_trade_envelope_additional_field(const std::string& trade_id,
                                        const std::string& sequence_number);

    /**
     * @brief Saves a trade envelope additional field (creates or updates).
     *
     * @param trade_envelope_additional_field The trade envelope additional field to save.
     * @throws std::exception on failure.
     */
    void save_trade_envelope_additional_field(
        const domain::trade_envelope_additional_field& trade_envelope_additional_field);

    /**
     * @brief Saves a batch of trade envelope additional fields.
     *
     * @param trade_envelope_additional_fields The trade envelope additional fields to save.
     * @throws std::exception on failure.
     */
    void save_trade_envelope_additional_fields(
        const std::vector<domain::trade_envelope_additional_field>&
            trade_envelope_additional_fields);

    /**
     * @brief Deletes a trade envelope additional field by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_envelope_additional_field(const std::string& trade_id,
                                                const std::string& sequence_number);

    /**
     * @brief Deletes trade envelope additional fields by their primary keys.
     */
    void delete_trade_envelope_additional_fields(const std::vector<std::string>& trade_ids,
                                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a trade envelope additional field.
     */
    std::vector<domain::trade_envelope_additional_field>
    get_trade_envelope_additional_field_history(const std::string& trade_id,
                                                const std::string& sequence_number);

private:
    context ctx_;
    repository::trade_envelope_additional_field_repository repo_;
};

}

#endif
