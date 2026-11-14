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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_PRODUCT_TYPE_DEAL_CLASS_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_PRODUCT_TYPE_DEAL_CLASS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_product_type_deal_class.hpp"
#include "ores.refdata.api/messaging/party_product_type_deal_class_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_product_type_deal_class_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party product type deal classes.
 *
 * Provides a higher-level interface for party product type deal class operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_product_type_deal_class_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_product_type_deal_class_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_product_type_deal_class_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_product_type_deal_class_service(const context& ctx);

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
    ::ores::refdata::messaging::list_party_product_type_deal_classes_response
    list_party_product_type_deal_classes(
        const ::ores::refdata::messaging::list_party_product_type_deal_classes_request& request);
    ::ores::refdata::messaging::get_party_product_type_deal_class_response
    get_party_product_type_deal_class(
        const ::ores::refdata::messaging::get_party_product_type_deal_class_request& request);
    ::ores::refdata::messaging::get_many_party_product_type_deal_classes_response
    get_many_party_product_type_deal_classes(
        const ::ores::refdata::messaging::get_many_party_product_type_deal_classes_request&
            request);
    ::ores::refdata::messaging::put_party_product_type_deal_class_response
    put_party_product_type_deal_class(
        const ::ores::refdata::messaging::put_party_product_type_deal_class_request& request);
    ::ores::refdata::messaging::put_many_party_product_type_deal_classes_response
    put_many_party_product_type_deal_classes(
        const ::ores::refdata::messaging::put_many_party_product_type_deal_classes_request&
            request);
    ::ores::refdata::messaging::delete_party_product_type_deal_class_response
    delete_party_product_type_deal_class(
        const ::ores::refdata::messaging::delete_party_product_type_deal_class_request& request);
    ::ores::refdata::messaging::delete_many_party_product_type_deal_classes_response
    delete_many_party_product_type_deal_classes(
        const ::ores::refdata::messaging::delete_many_party_product_type_deal_classes_request&
            request);
    ::ores::refdata::messaging::list_by_party_id_party_product_type_deal_classes_response
    list_by_party_id_party_product_type_deal_classes(
        const ::ores::refdata::messaging::list_by_party_id_party_product_type_deal_classes_request&
            request);
    ::ores::refdata::messaging::list_party_product_type_deal_class_versions_response
    list_party_product_type_deal_class_versions(
        const ::ores::refdata::messaging::list_party_product_type_deal_class_versions_request&
            request);
    ::ores::refdata::messaging::get_party_product_type_deal_class_version_response
    get_party_product_type_deal_class_version(
        const ::ores::refdata::messaging::get_party_product_type_deal_class_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists party product type deal classes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party product type deal classes for the requested page.
     */
    std::vector<domain::party_product_type_deal_class>
    list_party_product_type_deal_classes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party product type deal classes.
     *
     * @return Total number of active party product type deal classes.
     */
    std::uint32_t count_party_product_type_deal_classes();


    /**
     * @brief Lists party product type deal classes filtered by party_id, with pagination.
     *
     * @param party_id The party_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party product type deal classes for the requested page.
     */
    std::vector<domain::party_product_type_deal_class>
    list_party_product_type_deal_classes_by_party_id(const std::string& party_id,
                                                     std::uint32_t offset,
                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active party product type deal classes filtered by party_id.
     *
     * @param party_id The party_id to filter by.
     * @return Total number of matching party product type deal classes.
     */
    std::uint32_t count_party_product_type_deal_classes_by_party_id(const std::string& party_id);

    /**
     * @brief Lists party product type deal classes filtered by party_id, with pagination.
     *
     * @param party_id The party_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party product type deal classes for the requested page.
     */
    std::vector<domain::party_product_type_deal_class>
    list_party_product_type_deal_classes_by_party_id(const boost::uuids::uuid& party_id,
                                                     std::uint32_t offset,
                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active party product type deal classes filtered by party_id.
     *
     * @param party_id The party_id to filter by.
     * @return Total number of matching party product type deal classes.
     */
    std::uint32_t
    count_party_product_type_deal_classes_by_party_id(const boost::uuids::uuid& party_id);


    /**
     * @brief Retrieves a single party product type deal class as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party product type deal class at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_product_type_deal_class>
    get_party_product_type_deal_class_at_version(const boost::uuids::uuid& id,
                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single party product type deal class by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The party product type deal class if found, std::nullopt otherwise.
     */
    std::optional<domain::party_product_type_deal_class>
    get_party_product_type_deal_class(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of party product type deal classes by primary key.
     */
    std::vector<domain::party_product_type_deal_class>
    get_party_product_type_deal_classes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a party product type deal class (creates or updates).
     *
     * @param party_product_type_deal_class The party product type deal class to save.
     * @throws std::exception on failure.
     */
    void save_party_product_type_deal_class(
        const domain::party_product_type_deal_class& party_product_type_deal_class);

    /**
     * @brief Saves a batch of party product type deal classes.
     *
     * @param party_product_type_deal_classes The party product type deal classes to save.
     * @throws std::exception on failure.
     */
    void save_party_product_type_deal_classes(
        const std::vector<domain::party_product_type_deal_class>& party_product_type_deal_classes);

    /**
     * @brief Deletes a party product type deal class by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_party_product_type_deal_class(const boost::uuids::uuid& id);

    /**
     * @brief Deletes party product type deal classes by their primary keys.
     */
    void delete_party_product_type_deal_classes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party product type deal class.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::party_product_type_deal_class>
    get_party_product_type_deal_class_history(const std::string& id);

private:
    context ctx_;
    repository::party_product_type_deal_class_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::party_product_type_deal_class_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::party_product_type_deal_class& out);
};

}

#endif
