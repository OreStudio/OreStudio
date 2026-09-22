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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_currency_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currencies.
 *
 * Provides a higher-level interface for currency operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.currency_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_service(context ctx);

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
    messaging::list_currencies_response
    list_currencies(const messaging::list_currencies_request& request);
    messaging::get_currency_response get_currency(const messaging::get_currency_request& request);
    messaging::get_many_currencies_response
    get_many_currencies(const messaging::get_many_currencies_request& request);
    messaging::put_currency_response put_currency(const messaging::put_currency_request& request);
    messaging::put_many_currencies_response
    put_many_currencies(const messaging::put_many_currencies_request& request);
    messaging::delete_currency_response
    delete_currency(const messaging::delete_currency_request& request);
    messaging::delete_many_currencies_response
    delete_many_currencies(const messaging::delete_many_currencies_request& request);
    messaging::list_currency_versions_response
    list_currency_versions(const messaging::list_currency_versions_request& request);
    messaging::get_currency_version_response
    get_currency_version(const messaging::get_currency_version_request& request);
    /**@}*/

    /**
     * @brief Lists currencies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currencies for the requested page.
     */
    std::vector<domain::currency> list_currencies(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currencies.
     *
     * @return Total number of active currencies.
     */
    std::uint32_t count_currencies();


    /**
     * @brief Retrieves a single currency as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The currency at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency> get_currency_at_version(const std::string& iso_code,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single currency by its primary key.
     *
     * @return The currency if found, std::nullopt otherwise.
     */
    std::optional<domain::currency> get_currency(const std::string& iso_code);

    /**
     * @brief Retrieves a batch of currencies by primary key.
     */
    std::vector<domain::currency> get_currencies(const std::vector<std::string>& iso_codes);

    /**
     * @brief Saves a currency (creates or updates).
     *
     * @param currency The currency to save.
     * @throws std::exception on failure.
     */
    void save_currency(const domain::currency& currency);

    /**
     * @brief Saves a batch of currencies.
     *
     * @param currencies The currencies to save.
     * @throws std::exception on failure.
     */
    void save_currencies(const std::vector<domain::currency>& currencies);

    /**
     * @brief Deletes a currency by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_currency(const std::string& iso_code);

    /**
     * @brief Deletes currencies by their primary keys.
     */
    void delete_currencies(const std::vector<std::string>& iso_codes);

    /**
     * @brief Retrieves all historical versions of a currency.
     */
    std::vector<domain::currency> get_currency_history(const std::string& iso_code);

    /**
     * @brief Lists currencies visible to a specific party, with pagination.
     *
     * Uses the party_currencies junction to filter the full currency list
     * to only those the given party is permitted to see.
     *
     * @param party_id The UUID of the party.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of visible currencies for the requested page.
     */
    std::vector<domain::currency> list_currencies_for_party(const boost::uuids::uuid& party_id,
                                                            std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of currencies visible to a specific party.
     *
     * @param party_id The UUID of the party.
     * @return Number of currencies the party is permitted to see.
     */
    std::uint32_t count_currencies_for_party(const boost::uuids::uuid& party_id);

private:
    context ctx_;
    repository::currency_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::currency_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::currency& out);
    repository::party_currency_repository junction_repo_;
};

}

#endif
