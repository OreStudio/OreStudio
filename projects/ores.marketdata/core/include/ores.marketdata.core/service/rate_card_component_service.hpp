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
#ifndef ORES_MARKETDATA_CORE_SERVICE_RATE_CARD_COMPONENT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_RATE_CARD_COMPONENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/rate_card_component.hpp"
#include "ores.marketdata.api/messaging/rate_card_component_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/rate_card_component_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing rate card components.
 *
 * Provides a higher-level interface for rate card component operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT rate_card_component_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.rate_card_component_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a rate_card_component_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit rate_card_component_service(context ctx);

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
    ::ores::marketdata::messaging::list_rate_card_components_response list_rate_card_components(
        const ::ores::marketdata::messaging::list_rate_card_components_request& request);
    ::ores::marketdata::messaging::get_rate_card_component_response get_rate_card_component(
        const ::ores::marketdata::messaging::get_rate_card_component_request& request);
    ::ores::marketdata::messaging::get_many_rate_card_components_response
    get_many_rate_card_components(
        const ::ores::marketdata::messaging::get_many_rate_card_components_request& request);
    ::ores::marketdata::messaging::put_rate_card_component_response put_rate_card_component(
        const ::ores::marketdata::messaging::put_rate_card_component_request& request);
    ::ores::marketdata::messaging::put_many_rate_card_components_response
    put_many_rate_card_components(
        const ::ores::marketdata::messaging::put_many_rate_card_components_request& request);
    ::ores::marketdata::messaging::delete_rate_card_component_response delete_rate_card_component(
        const ::ores::marketdata::messaging::delete_rate_card_component_request& request);
    ::ores::marketdata::messaging::delete_many_rate_card_components_response
    delete_many_rate_card_components(
        const ::ores::marketdata::messaging::delete_many_rate_card_components_request& request);
    ::ores::marketdata::messaging::list_rate_card_component_versions_response
    list_rate_card_component_versions(
        const ::ores::marketdata::messaging::list_rate_card_component_versions_request& request);
    ::ores::marketdata::messaging::get_rate_card_component_version_response
    get_rate_card_component_version(
        const ::ores::marketdata::messaging::get_rate_card_component_version_request& request);
    /**@}*/

    /**
     * @brief Lists rate card components with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of rate card components for the requested page.
     */
    std::vector<domain::rate_card_component> list_rate_card_components(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active rate card components.
     *
     * @return Total number of active rate card components.
     */
    std::uint32_t count_rate_card_components();


    /**
     * @brief Retrieves a single rate card component as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The rate card component at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::rate_card_component>
    get_rate_card_component_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single rate card component by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The rate card component if found, std::nullopt otherwise.
     */
    std::optional<domain::rate_card_component>
    get_rate_card_component(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single rate card component by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The rate card component if found, std::nullopt otherwise.
     */
    std::optional<domain::rate_card_component>
    get_rate_card_component_by_driver(const std::string& driver);

    /**
     * @brief Retrieves a batch of rate card components by primary key.
     */
    std::vector<domain::rate_card_component>
    get_rate_card_components(const std::vector<std::string>& ids);

    /**
     * @brief Saves a rate card component (creates or updates).
     *
     * @param rate_card_component The rate card component to save.
     * @throws std::exception on failure.
     */
    void save_rate_card_component(const domain::rate_card_component& rate_card_component);

    /**
     * @brief Saves a batch of rate card components.
     *
     * @param rate_card_components The rate card components to save.
     * @throws std::exception on failure.
     */
    void
    save_rate_card_components(const std::vector<domain::rate_card_component>& rate_card_components);

    /**
     * @brief Deletes a rate card component by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rate_card_component(const boost::uuids::uuid& id);

    /**
     * @brief Deletes rate card components by their primary keys.
     */
    void delete_rate_card_components(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a rate card component.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::rate_card_component>
    get_rate_card_component_history(const std::string& key);

private:
    context ctx_;
    repository::rate_card_component_repository repo_;

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
    prepare_change(const ::ores::marketdata::messaging::rate_card_component_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::rate_card_component& out);
};

}

#endif
