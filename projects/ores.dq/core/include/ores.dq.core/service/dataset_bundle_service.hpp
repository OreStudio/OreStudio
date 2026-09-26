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
#ifndef ORES_DQ_CORE_SERVICE_DATASET_BUNDLE_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATASET_BUNDLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/dataset_bundle.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/dataset_bundle_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing dataset bundles.
 *
 * Provides a higher-level interface for dataset bundle operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT dataset_bundle_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.dataset_bundle_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dataset_bundle_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit dataset_bundle_service(context ctx);

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
    messaging::list_dataset_bundles_response
    list_dataset_bundles(const messaging::list_dataset_bundles_request& request);
    messaging::get_dataset_bundle_response
    get_dataset_bundle(const messaging::get_dataset_bundle_request& request);
    messaging::get_many_dataset_bundles_response
    get_many_dataset_bundles(const messaging::get_many_dataset_bundles_request& request);
    messaging::put_dataset_bundle_response
    put_dataset_bundle(const messaging::put_dataset_bundle_request& request);
    messaging::put_many_dataset_bundles_response
    put_many_dataset_bundles(const messaging::put_many_dataset_bundles_request& request);
    messaging::delete_dataset_bundle_response
    delete_dataset_bundle(const messaging::delete_dataset_bundle_request& request);
    messaging::delete_many_dataset_bundles_response
    delete_many_dataset_bundles(const messaging::delete_many_dataset_bundles_request& request);
    messaging::list_dataset_bundle_versions_response
    list_dataset_bundle_versions(const messaging::list_dataset_bundle_versions_request& request);
    messaging::get_dataset_bundle_version_response
    get_dataset_bundle_version(const messaging::get_dataset_bundle_version_request& request);
    /**@}*/

    /**
     * @brief Lists dataset bundles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of dataset bundles for the requested page.
     */
    std::vector<domain::dataset_bundle> list_bundles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active dataset bundles.
     *
     * @return Total number of active dataset bundles.
     */
    std::uint32_t count_bundles();


    /**
     * @brief Retrieves a single dataset bundle as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The dataset bundle at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::dataset_bundle> get_bundle_at_version(const boost::uuids::uuid& id,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single dataset bundle by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The dataset bundle if found, std::nullopt otherwise.
     */
    std::optional<domain::dataset_bundle> get_bundle(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single dataset bundle by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The dataset bundle if found, std::nullopt otherwise.
     */
    std::optional<domain::dataset_bundle> get_bundle_by_code(const std::string& code);

    /**
     * @brief Retrieves a batch of dataset bundles by primary key.
     */
    std::vector<domain::dataset_bundle> get_bundles(const std::vector<std::string>& ids);

    /**
     * @brief Saves a dataset bundle (creates or updates).
     *
     * @param bundle The dataset bundle to save.
     * @throws std::exception on failure.
     */
    void save_bundle(const domain::dataset_bundle& bundle);

    /**
     * @brief Saves a batch of dataset bundles.
     *
     * @param bundles The dataset bundles to save.
     * @throws std::exception on failure.
     */
    void save_bundles(const std::vector<domain::dataset_bundle>& bundles);

    /**
     * @brief Deletes a dataset bundle by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bundle(const boost::uuids::uuid& id);

    /**
     * @brief Deletes dataset bundles by their primary keys.
     */
    void delete_bundles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a dataset bundle.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::dataset_bundle> get_bundle_history(const std::string& key);

private:
    context ctx_;
    repository::dataset_bundle_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::dataset_bundle_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::dataset_bundle& out);
};

}

#endif
