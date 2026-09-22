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
#ifndef ORES_REFDATA_CORE_SERVICE_PAYMENT_FREQUENCY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PAYMENT_FREQUENCY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/payment_frequency.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/payment_frequency_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing payment frequencies.
 *
 * Provides a higher-level interface for payment frequency operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT payment_frequency_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.payment_frequency_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a payment_frequency_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit payment_frequency_service(context ctx);

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
    messaging::list_payment_frequencies_response
    list_payment_frequencies(const messaging::list_payment_frequencies_request& request);
    messaging::get_payment_frequency_response
    get_payment_frequency(const messaging::get_payment_frequency_request& request);
    messaging::get_many_payment_frequencies_response
    get_many_payment_frequencies(const messaging::get_many_payment_frequencies_request& request);
    messaging::put_payment_frequency_response
    put_payment_frequency(const messaging::put_payment_frequency_request& request);
    messaging::put_many_payment_frequencies_response
    put_many_payment_frequencies(const messaging::put_many_payment_frequencies_request& request);
    messaging::delete_payment_frequency_response
    delete_payment_frequency(const messaging::delete_payment_frequency_request& request);
    messaging::delete_many_payment_frequencies_response delete_many_payment_frequencies(
        const messaging::delete_many_payment_frequencies_request& request);
    messaging::list_payment_frequency_versions_response list_payment_frequency_versions(
        const messaging::list_payment_frequency_versions_request& request);
    messaging::get_payment_frequency_version_response
    get_payment_frequency_version(const messaging::get_payment_frequency_version_request& request);
    /**@}*/

    /**
     * @brief Lists payment frequencies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of payment frequencies for the requested page.
     */
    std::vector<domain::payment_frequency> list_payment_frequencies(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active payment frequencies.
     *
     * @return Total number of active payment frequencies.
     */
    std::uint32_t count_payment_frequencies();


    /**
     * @brief Retrieves a single payment frequency as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The payment frequency at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::payment_frequency>
    get_payment_frequency_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single payment frequency by its primary key.
     *
     * @return The payment frequency if found, std::nullopt otherwise.
     */
    std::optional<domain::payment_frequency> get_payment_frequency(const std::string& code);

    /**
     * @brief Retrieves a batch of payment frequencies by primary key.
     */
    std::vector<domain::payment_frequency>
    get_payment_frequencies(const std::vector<std::string>& codes);

    /**
     * @brief Saves a payment frequency (creates or updates).
     *
     * @param payment_frequency The payment frequency to save.
     * @throws std::exception on failure.
     */
    void save_payment_frequency(const domain::payment_frequency& payment_frequency);

    /**
     * @brief Saves a batch of payment frequencies.
     *
     * @param payment_frequencies The payment frequencies to save.
     * @throws std::exception on failure.
     */
    void
    save_payment_frequencies(const std::vector<domain::payment_frequency>& payment_frequencies);

    /**
     * @brief Deletes a payment frequency by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_payment_frequency(const std::string& code);

    /**
     * @brief Deletes payment frequencies by their primary keys.
     */
    void delete_payment_frequencies(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a payment frequency.
     */
    std::vector<domain::payment_frequency> get_payment_frequency_history(const std::string& code);

private:
    context ctx_;
    repository::payment_frequency_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::payment_frequency_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::payment_frequency& out);
};

}

#endif
