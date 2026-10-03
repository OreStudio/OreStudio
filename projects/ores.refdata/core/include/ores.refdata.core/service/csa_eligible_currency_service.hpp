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
#ifndef ORES_REFDATA_CORE_SERVICE_CSA_ELIGIBLE_CURRENCY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CSA_ELIGIBLE_CURRENCY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/csa_eligible_currency.hpp"
#include "ores.refdata.api/messaging/csa_eligible_currency_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/csa_eligible_currency_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing CSA eligible currencies.
 *
 * Provides a higher-level interface for CSA eligible currency operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT csa_eligible_currency_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.csa_eligible_currency_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a csa_eligible_currency_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit csa_eligible_currency_service(context ctx);

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
    messaging::list_csa_eligible_currencies_response
    list_csa_eligible_currencies(const messaging::list_csa_eligible_currencies_request& request);
    messaging::get_csa_eligible_currency_response
    get_csa_eligible_currency(const messaging::get_csa_eligible_currency_request& request);
    messaging::get_many_csa_eligible_currencies_response get_many_csa_eligible_currencies(
        const messaging::get_many_csa_eligible_currencies_request& request);
    messaging::put_csa_eligible_currency_response
    put_csa_eligible_currency(const messaging::put_csa_eligible_currency_request& request);
    messaging::put_many_csa_eligible_currencies_response put_many_csa_eligible_currencies(
        const messaging::put_many_csa_eligible_currencies_request& request);
    messaging::delete_csa_eligible_currency_response
    delete_csa_eligible_currency(const messaging::delete_csa_eligible_currency_request& request);
    messaging::delete_many_csa_eligible_currencies_response delete_many_csa_eligible_currencies(
        const messaging::delete_many_csa_eligible_currencies_request& request);
    messaging::list_by_csa_id_csa_eligible_currencies_response
    list_by_csa_id_csa_eligible_currencies(
        const messaging::list_by_csa_id_csa_eligible_currencies_request& request);
    messaging::list_csa_eligible_currency_versions_response list_csa_eligible_currency_versions(
        const messaging::list_csa_eligible_currency_versions_request& request);
    messaging::get_csa_eligible_currency_version_response get_csa_eligible_currency_version(
        const messaging::get_csa_eligible_currency_version_request& request);
    /**@}*/

    /**
     * @brief Lists CSA eligible currencies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CSA eligible currencies for the requested page.
     */
    std::vector<domain::csa_eligible_currency> list_csa_eligible_currencies(std::uint32_t offset,
                                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSA eligible currencies.
     *
     * @return Total number of active CSA eligible currencies.
     */
    std::uint32_t count_csa_eligible_currencies();


    /**
     * @brief Lists CSA eligible currencies filtered by csa_id, with pagination.
     *
     * @param csa_id The csa_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching CSA eligible currencies for the requested page.
     */
    std::vector<domain::csa_eligible_currency> list_csa_eligible_currencies_by_csa_id(
        const std::string& csa_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSA eligible currencies filtered by csa_id.
     *
     * @param csa_id The csa_id to filter by.
     * @return Total number of matching CSA eligible currencies.
     */
    std::uint32_t count_csa_eligible_currencies_by_csa_id(const std::string& csa_id);

    /**
     * @brief Lists CSA eligible currencies filtered by csa_id, with pagination.
     *
     * @param csa_id The csa_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching CSA eligible currencies for the requested page.
     */
    std::vector<domain::csa_eligible_currency> list_csa_eligible_currencies_by_csa_id(
        const boost::uuids::uuid& csa_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSA eligible currencies filtered by csa_id.
     *
     * @param csa_id The csa_id to filter by.
     * @return Total number of matching CSA eligible currencies.
     */
    std::uint32_t count_csa_eligible_currencies_by_csa_id(const boost::uuids::uuid& csa_id);


    /**
     * @brief Retrieves a single CSA eligible currency as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The CSA eligible currency at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::csa_eligible_currency>
    get_csa_eligible_currency_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single CSA eligible currency by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The CSA eligible currency if found, std::nullopt otherwise.
     */
    std::optional<domain::csa_eligible_currency>
    get_csa_eligible_currency(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single CSA eligible currency by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The CSA eligible currency if found, std::nullopt otherwise.
     */
    std::optional<domain::csa_eligible_currency>
    get_csa_eligible_currency_by_currency_code(const std::string& currency_code);

    /**
     * @brief Retrieves a single CSA eligible currency by its uuid primary key.
     *
     * @return The CSA eligible currency if found, std::nullopt otherwise.
     */
    std::optional<domain::csa_eligible_currency>
    find_csa_eligible_currency(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of CSA eligible currencies by primary key.
     */
    std::vector<domain::csa_eligible_currency>
    get_csa_eligible_currencies(const std::vector<std::string>& ids);

    /**
     * @brief Saves a CSA eligible currency (creates or updates).
     *
     * @param csa_eligible_currency The CSA eligible currency to save.
     * @throws std::exception on failure.
     */
    void save_csa_eligible_currency(const domain::csa_eligible_currency& csa_eligible_currency);

    /**
     * @brief Saves a batch of CSA eligible currencies.
     *
     * @param csa_eligible_currencies The CSA eligible currencies to save.
     * @throws std::exception on failure.
     */
    void save_csa_eligible_currencies(
        const std::vector<domain::csa_eligible_currency>& csa_eligible_currencies);

    /**
     * @brief Deletes a CSA eligible currency by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_csa_eligible_currency(const boost::uuids::uuid& id);

    /**
     * @brief Removes a CSA eligible currency by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_csa_eligible_currency(const boost::uuids::uuid& id);

    /**
     * @brief Deletes CSA eligible currencies by their primary keys.
     */
    void delete_csa_eligible_currencies(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CSA eligible currency.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::csa_eligible_currency>
    get_csa_eligible_currency_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a CSA eligible currency
     * by its uuid primary key.
     */
    std::vector<domain::csa_eligible_currency>
    get_csa_eligible_currency_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::csa_eligible_currency_repository repo_;

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
    prepare_change(const messaging::csa_eligible_currency_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::csa_eligible_currency& out);
};

}

#endif
