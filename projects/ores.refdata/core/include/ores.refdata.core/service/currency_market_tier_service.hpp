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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_MARKET_TIER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_MARKET_TIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_market_tier.hpp"
#include "ores.refdata.api/messaging/currency_market_tier_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_market_tier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency market tiers.
 *
 * Provides a higher-level interface for currency market tier operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_market_tier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_market_tier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_market_tier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_market_tier_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_currency_market_tiers_response
    list_currency_market_tiers(const messaging::list_currency_market_tiers_request& request);
    messaging::get_currency_market_tier_response
    get_currency_market_tier(const messaging::get_currency_market_tier_request& request);
    messaging::get_many_currency_market_tiers_response get_many_currency_market_tiers(
        const messaging::get_many_currency_market_tiers_request& request);
    messaging::put_currency_market_tier_response
    put_currency_market_tier(const messaging::put_currency_market_tier_request& request);
    messaging::put_many_currency_market_tiers_response put_many_currency_market_tiers(
        const messaging::put_many_currency_market_tiers_request& request);
    messaging::delete_currency_market_tier_response
    delete_currency_market_tier(const messaging::delete_currency_market_tier_request& request);
    messaging::delete_many_currency_market_tiers_response delete_many_currency_market_tiers(
        const messaging::delete_many_currency_market_tiers_request& request);
    messaging::list_currency_market_tier_versions_response list_currency_market_tier_versions(
        const messaging::list_currency_market_tier_versions_request& request);
    messaging::get_currency_market_tier_version_response get_currency_market_tier_version(
        const messaging::get_currency_market_tier_version_request& request);
    /**@}*/

    /**
     * @brief Lists currency market tiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency market tiers for the requested page.
     */
    std::vector<domain::currency_market_tier> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency market tiers.
     *
     * @return Total number of active currency market tiers.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single currency market tier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The currency market tier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_market_tier> get_type_at_version(const std::string& code,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single currency market tier by its primary key.
     *
     * @return The currency market tier if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_market_tier> get_type(const std::string& code);

    /**
     * @brief Retrieves a batch of currency market tiers by primary key.
     */
    std::vector<domain::currency_market_tier> get_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a currency market tier (creates or updates).
     *
     * @param type The currency market tier to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::currency_market_tier& type);

    /**
     * @brief Saves a batch of currency market tiers.
     *
     * @param types The currency market tiers to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::currency_market_tier>& types);

    /**
     * @brief Deletes a currency market tier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes currency market tiers by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a currency market tier.
     */
    std::vector<domain::currency_market_tier> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::currency_market_tier_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const messaging::currency_market_tier_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::currency_market_tier& out);
};

}

#endif
