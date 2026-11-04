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
#ifndef ORES_REFDATA_CORE_SERVICE_COMMODITY_FUTURE_OPTION_EXPIRY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COMMODITY_FUTURE_OPTION_EXPIRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/commodity_future_option_expiry.hpp"
#include "ores.refdata.api/messaging/commodity_future_option_expiry_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/commodity_future_option_expiry_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing commodity future option expiries.
 *
 * Provides a higher-level interface for commodity future option expiry operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT commodity_future_option_expiry_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.commodity_future_option_expiry_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a commodity_future_option_expiry_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit commodity_future_option_expiry_service(context ctx);

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
    ::ores::refdata::messaging::list_commodity_future_option_expiries_response
    list_commodity_future_option_expiries(
        const ::ores::refdata::messaging::list_commodity_future_option_expiries_request& request);
    ::ores::refdata::messaging::get_commodity_future_option_expiry_response
    get_commodity_future_option_expiry(
        const ::ores::refdata::messaging::get_commodity_future_option_expiry_request& request);
    ::ores::refdata::messaging::get_many_commodity_future_option_expiries_response
    get_many_commodity_future_option_expiries(
        const ::ores::refdata::messaging::get_many_commodity_future_option_expiries_request&
            request);
    ::ores::refdata::messaging::put_commodity_future_option_expiry_response
    put_commodity_future_option_expiry(
        const ::ores::refdata::messaging::put_commodity_future_option_expiry_request& request);
    ::ores::refdata::messaging::put_many_commodity_future_option_expiries_response
    put_many_commodity_future_option_expiries(
        const ::ores::refdata::messaging::put_many_commodity_future_option_expiries_request&
            request);
    ::ores::refdata::messaging::delete_commodity_future_option_expiry_response
    delete_commodity_future_option_expiry(
        const ::ores::refdata::messaging::delete_commodity_future_option_expiry_request& request);
    ::ores::refdata::messaging::delete_many_commodity_future_option_expiries_response
    delete_many_commodity_future_option_expiries(
        const ::ores::refdata::messaging::delete_many_commodity_future_option_expiries_request&
            request);
    ::ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_response
    list_by_convention_id_commodity_future_option_expiries(
        const ::ores::refdata::messaging::
            list_by_convention_id_commodity_future_option_expiries_request& request);
    ::ores::refdata::messaging::list_commodity_future_option_expiry_versions_response
    list_commodity_future_option_expiry_versions(
        const ::ores::refdata::messaging::list_commodity_future_option_expiry_versions_request&
            request);
    ::ores::refdata::messaging::get_commodity_future_option_expiry_version_response
    get_commodity_future_option_expiry_version(
        const ::ores::refdata::messaging::get_commodity_future_option_expiry_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists commodity future option expiries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of commodity future option expiries for the requested page.
     */
    std::vector<domain::commodity_future_option_expiry>
    list_commodity_future_option_expiries(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active commodity future option expiries.
     *
     * @return Total number of active commodity future option expiries.
     */
    std::uint32_t count_commodity_future_option_expiries();


    /**
     * @brief Lists commodity future option expiries filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching commodity future option expiries for the requested page.
     */
    std::vector<domain::commodity_future_option_expiry>
    list_commodity_future_option_expiries_by_convention_id(const std::string& convention_id,
                                                           std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active commodity future option expiries filtered by
     * convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching commodity future option expiries.
     */
    std::uint32_t
    count_commodity_future_option_expiries_by_convention_id(const std::string& convention_id);

    /**
     * @brief Lists commodity future option expiries filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching commodity future option expiries for the requested page.
     */
    std::vector<domain::commodity_future_option_expiry>
    list_commodity_future_option_expiries_by_convention_id(const boost::uuids::uuid& convention_id,
                                                           std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active commodity future option expiries filtered by
     * convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching commodity future option expiries.
     */
    std::uint32_t count_commodity_future_option_expiries_by_convention_id(
        const boost::uuids::uuid& convention_id);


    /**
     * @brief Lists commodity future option expiries filtered by convention_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param convention_id The convention_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching commodity future option expiries.
     */
    std::vector<domain::commodity_future_option_expiry>
    list_commodity_future_option_expiries_by_convention_id_as_of(
        const std::string& convention_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single commodity future option expiry as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The commodity future option expiry at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_future_option_expiry>
    get_commodity_future_option_expiry_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single commodity future option expiry by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The commodity future option expiry if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_future_option_expiry>
    get_commodity_future_option_expiry(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of commodity future option expiries by primary key.
     */
    std::vector<domain::commodity_future_option_expiry>
    get_commodity_future_option_expiries(const std::vector<std::string>& ids);

    /**
     * @brief Saves a commodity future option expiry (creates or updates).
     *
     * @param commodity_future_option_expiry The commodity future option expiry to save.
     * @throws std::exception on failure.
     */
    void save_commodity_future_option_expiry(
        const domain::commodity_future_option_expiry& commodity_future_option_expiry);

    /**
     * @brief Saves a batch of commodity future option expiries.
     *
     * @param commodity_future_option_expiries The commodity future option expiries to save.
     * @throws std::exception on failure.
     */
    void
    save_commodity_future_option_expiries(const std::vector<domain::commodity_future_option_expiry>&
                                              commodity_future_option_expiries);

    /**
     * @brief Deletes a commodity future option expiry by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_commodity_future_option_expiry(const boost::uuids::uuid& id);

    /**
     * @brief Deletes commodity future option expiries by their primary keys.
     */
    void delete_commodity_future_option_expiries(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a commodity future option expiry.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::commodity_future_option_expiry>
    get_commodity_future_option_expiry_history(const std::string& id);

private:
    context ctx_;
    repository::commodity_future_option_expiry_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::commodity_future_option_expiry_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::commodity_future_option_expiry& out);
};

}

#endif
