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
#ifndef ORES_COMPUTE_CORE_SERVICE_BATCH_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_BATCH_SERVICE_HPP

#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute batches.
 *
 * Provides a higher-level interface for compute batch operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT batch_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.batch_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a batch_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit batch_service(context ctx);

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
    messaging::list_batches_response list_batches(const messaging::list_batches_request& request);
    messaging::get_batch_response get_batch(const messaging::get_batch_request& request);
    messaging::get_many_batches_response
    get_many_batches(const messaging::get_many_batches_request& request);
    messaging::put_batch_response put_batch(const messaging::put_batch_request& request);
    messaging::put_many_batches_response
    put_many_batches(const messaging::put_many_batches_request& request);
    messaging::delete_batch_response delete_batch(const messaging::delete_batch_request& request);
    messaging::delete_many_batches_response
    delete_many_batches(const messaging::delete_many_batches_request& request);
    messaging::list_batch_versions_response
    list_batch_versions(const messaging::list_batch_versions_request& request);
    messaging::get_batch_version_response
    get_batch_version(const messaging::get_batch_version_request& request);
    /**@}*/

    /**
     * @brief Lists compute batches with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute batches for the requested page.
     */
    std::vector<domain::batch> list_batches(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute batches.
     *
     * @return Total number of active compute batches.
     */
    std::uint32_t count_batches();


    /**
     * @brief Retrieves a single compute batch as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute batch at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::batch> get_batch_at_version(const boost::uuids::uuid& id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single compute batch by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The compute batch if found, std::nullopt otherwise.
     */
    std::optional<domain::batch> get_batch(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single compute batch by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The compute batch if found, std::nullopt otherwise.
     */
    std::optional<domain::batch> get_batch_by_external_ref(const std::string& external_ref);

    /**
     * @brief Retrieves a batch of compute batches by primary key.
     */
    std::vector<domain::batch> get_batches(const std::vector<std::string>& ids);

    /**
     * @brief Saves a compute batch (creates or updates).
     *
     * @param batch The compute batch to save.
     * @throws std::exception on failure.
     */
    void save_batch(const domain::batch& batch);

    /**
     * @brief Saves a batch of compute batches.
     *
     * @param batches The compute batches to save.
     * @throws std::exception on failure.
     */
    void save_batches(const std::vector<domain::batch>& batches);

    /**
     * @brief Deletes a compute batch by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_batch(const boost::uuids::uuid& id);

    /**
     * @brief Deletes compute batches by their primary keys.
     */
    void delete_batches(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute batch.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::batch> get_batch_history(const std::string& key);

private:
    context ctx_;
    repository::batch_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::batch_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::batch& out);
};

}

#endif
