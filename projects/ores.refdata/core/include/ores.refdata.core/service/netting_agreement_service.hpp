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
#ifndef ORES_REFDATA_CORE_SERVICE_NETTING_AGREEMENT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_NETTING_AGREEMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/netting_agreement.hpp"
#include "ores.refdata.api/messaging/netting_agreement_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/netting_agreement_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing netting agreements.
 *
 * Provides a higher-level interface for netting agreement operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT netting_agreement_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.netting_agreement_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a netting_agreement_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit netting_agreement_service(context ctx);

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
    messaging::list_netting_agreements_response
    list_netting_agreements(const messaging::list_netting_agreements_request& request);
    messaging::get_netting_agreement_response
    get_netting_agreement(const messaging::get_netting_agreement_request& request);
    messaging::get_many_netting_agreements_response
    get_many_netting_agreements(const messaging::get_many_netting_agreements_request& request);
    messaging::put_netting_agreement_response
    put_netting_agreement(const messaging::put_netting_agreement_request& request);
    messaging::put_many_netting_agreements_response
    put_many_netting_agreements(const messaging::put_many_netting_agreements_request& request);
    messaging::delete_netting_agreement_response
    delete_netting_agreement(const messaging::delete_netting_agreement_request& request);
    messaging::delete_many_netting_agreements_response delete_many_netting_agreements(
        const messaging::delete_many_netting_agreements_request& request);
    messaging::list_by_counterparty_id_netting_agreements_response
    list_by_counterparty_id_netting_agreements(
        const messaging::list_by_counterparty_id_netting_agreements_request& request);
    messaging::list_netting_agreement_versions_response list_netting_agreement_versions(
        const messaging::list_netting_agreement_versions_request& request);
    messaging::get_netting_agreement_version_response
    get_netting_agreement_version(const messaging::get_netting_agreement_version_request& request);
    /**@}*/

    /**
     * @brief Lists netting agreements with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of netting agreements for the requested page.
     */
    std::vector<domain::netting_agreement> list_netting_agreements(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting agreements.
     *
     * @return Total number of active netting agreements.
     */
    std::uint32_t count_netting_agreements();


    /**
     * @brief Lists netting agreements filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching netting agreements for the requested page.
     */
    std::vector<domain::netting_agreement> list_netting_agreements_by_counterparty_id(
        const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting agreements filtered by counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching netting agreements.
     */
    std::uint32_t count_netting_agreements_by_counterparty_id(const std::string& counterparty_id);

    /**
     * @brief Lists netting agreements filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching netting agreements for the requested page.
     */
    std::vector<domain::netting_agreement> list_netting_agreements_by_counterparty_id(
        const boost::uuids::uuid& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting agreements filtered by counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching netting agreements.
     */
    std::uint32_t
    count_netting_agreements_by_counterparty_id(const boost::uuids::uuid& counterparty_id);


    /**
     * @brief Retrieves a single netting agreement as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The netting agreement at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_agreement>
    get_netting_agreement_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single netting agreement by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The netting agreement if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_agreement> get_netting_agreement(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single netting agreement by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The netting agreement if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_agreement>
    get_netting_agreement_by_agreement_number(const std::string& agreement_number);

    /**
     * @brief Retrieves a single netting agreement by its uuid primary key.
     *
     * @return The netting agreement if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_agreement> find_netting_agreement(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of netting agreements by primary key.
     */
    std::vector<domain::netting_agreement>
    get_netting_agreements(const std::vector<std::string>& ids);

    /**
     * @brief Saves a netting agreement (creates or updates).
     *
     * @param netting_agreement The netting agreement to save.
     * @throws std::exception on failure.
     */
    void save_netting_agreement(const domain::netting_agreement& netting_agreement);

    /**
     * @brief Saves a batch of netting agreements.
     *
     * @param netting_agreements The netting agreements to save.
     * @throws std::exception on failure.
     */
    void save_netting_agreements(const std::vector<domain::netting_agreement>& netting_agreements);

    /**
     * @brief Deletes a netting agreement by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_netting_agreement(const boost::uuids::uuid& id);

    /**
     * @brief Removes a netting agreement by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_netting_agreement(const boost::uuids::uuid& id);

    /**
     * @brief Deletes netting agreements by their primary keys.
     */
    void delete_netting_agreements(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a netting agreement.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::netting_agreement> get_netting_agreement_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a netting agreement
     * by its uuid primary key.
     */
    std::vector<domain::netting_agreement>
    get_netting_agreement_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::netting_agreement_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::netting_agreement_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::netting_agreement& out);
};

}

#endif
