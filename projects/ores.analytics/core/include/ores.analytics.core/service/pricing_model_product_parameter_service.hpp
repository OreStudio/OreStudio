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
#ifndef ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP

#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing pricing model product parameters.
 *
 * Provides a higher-level interface for pricing model product parameter operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT pricing_model_product_parameter_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_product_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a pricing_model_product_parameter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit pricing_model_product_parameter_service(context ctx);

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
    messaging::list_pricing_model_product_parameters_response list_pricing_model_product_parameters(
        const messaging::list_pricing_model_product_parameters_request& request);
    messaging::get_pricing_model_product_parameter_response get_pricing_model_product_parameter(
        const messaging::get_pricing_model_product_parameter_request& request);
    messaging::get_many_pricing_model_product_parameters_response
    get_many_pricing_model_product_parameters(
        const messaging::get_many_pricing_model_product_parameters_request& request);
    messaging::put_pricing_model_product_parameter_response put_pricing_model_product_parameter(
        const messaging::put_pricing_model_product_parameter_request& request);
    messaging::put_many_pricing_model_product_parameters_response
    put_many_pricing_model_product_parameters(
        const messaging::put_many_pricing_model_product_parameters_request& request);
    messaging::delete_pricing_model_product_parameter_response
    delete_pricing_model_product_parameter(
        const messaging::delete_pricing_model_product_parameter_request& request);
    messaging::delete_many_pricing_model_product_parameters_response
    delete_many_pricing_model_product_parameters(
        const messaging::delete_many_pricing_model_product_parameters_request& request);
    messaging::list_pricing_model_product_parameter_versions_response
    list_pricing_model_product_parameter_versions(
        const messaging::list_pricing_model_product_parameter_versions_request& request);
    messaging::get_pricing_model_product_parameter_version_response
    get_pricing_model_product_parameter_version(
        const messaging::get_pricing_model_product_parameter_version_request& request);
    /**@}*/

    /**
     * @brief Lists pricing model product parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of pricing model product parameters for the requested page.
     */
    std::vector<domain::pricing_model_product_parameter> list_parameters(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active pricing model product parameters.
     *
     * @return Total number of active pricing model product parameters.
     */
    std::uint32_t count_parameters();


    /**
     * @brief Retrieves a single pricing model product parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The pricing model product parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    get_parameter_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single pricing model product parameter by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The pricing model product parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    get_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single pricing model product parameter by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The pricing model product parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    get_parameter_by_parameter_name(const std::string& parameter_name);

    /**
     * @brief Retrieves a single pricing model product parameter by its uuid primary key.
     *
     * @return The pricing model product parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    find_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of pricing model product parameters by primary key.
     */
    std::vector<domain::pricing_model_product_parameter>
    get_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Saves a pricing model product parameter (creates or updates).
     *
     * @param parameter The pricing model product parameter to save.
     * @throws std::exception on failure.
     */
    void save_parameter(const domain::pricing_model_product_parameter& parameter);

    /**
     * @brief Saves a batch of pricing model product parameters.
     *
     * @param parameters The pricing model product parameters to save.
     * @throws std::exception on failure.
     */
    void save_parameters(const std::vector<domain::pricing_model_product_parameter>& parameters);

    /**
     * @brief Deletes a pricing model product parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Removes a pricing model product parameter by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Deletes pricing model product parameters by their primary keys.
     */
    void delete_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a pricing model product parameter.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::pricing_model_product_parameter>
    get_parameter_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a pricing model product parameter
     * by its uuid primary key.
     */
    std::vector<domain::pricing_model_product_parameter>
    get_parameter_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::pricing_model_product_parameter_repository repo_;

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
    prepare_change(const messaging::pricing_model_product_parameter_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::pricing_model_product_parameter& out);
};

}

#endif
