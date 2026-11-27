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
#ifndef ORES_VAULT_SERVICE_APP_PROVISIONING_HPP
#define ORES_VAULT_SERVICE_APP_PROVISIONING_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.vault.service/export.hpp"
#include <iosfwd>
#include <string>
#include <string_view>
#include <vector>

namespace ores::vault::service::app {

/// One consumer the deployment registers, and the account it runs as.
struct provisioned_consumer {
    std::string name;
    std::string actor;
    std::string public_key;
};

/// One value the deployment stores, and the consumer that may resolve it.
struct provisioned_secret {
    std::string name;
    std::string owner;
    std::string value;
};

/**
 * @brief The custody material a deployment states in one document.
 *
 * One document rather than one call per entry, because the deployment's own
 * step runs once, before any service holds a token, and reads it from a pipe.
 * Consumers are written before secrets, so a value registered beside a key that
 * is not yet there is still owned by a consumer the vault knows.
 */
struct ORES_VAULT_SERVICE_EXPORT provision_document {
    std::vector<provisioned_consumer> consumers;
    std::vector<provisioned_secret> secrets;

    /**
     * @brief Reads the document the JSON text states.
     *
     * @param text The document.
     * @return Its entries, consumers first.
     *
     * @throws std::runtime_error If the text does not parse, or an entry lacks
     * a field, or a field is not a string.
     */
    [[nodiscard]] static provision_document read(std::string_view text);
};

/**
 * @brief Writes the custody material a deployment states, then returns.
 *
 * This runs before any service holds a token, so it is the one act on the vault
 * that no caller authenticates. It exists for a reason specific to the boot: the
 * first value a deployment stores is the password of the service that issues
 * the tokens a write would need, and the circle can only be broken by the
 * process that already holds the key encryption key. See
 * =lib/doc/knowledge/security/secure_introduction_of_a_workload_to_the_vault.org=.
 *
 * The document names the consumers the deployment registers and the values it
 * stores for them. It arrives on standard input or in a file the deployment
 * keeps nowhere, because a document of plaintext passwords on disk is the shape
 * this step exists to remove.
 *
 * Every entry is reported on @p out as it is written, and a failure is reported
 * on @p err and leaves the remaining entries unwritten.
 *
 * @param ctx The context the entries are written in, which must be the system
 * tenant: what a deployment provisions belongs to it and not to any tenant it
 * serves.
 * @param source The file to read, or ="-" for standard input.
 * @param out Where each written entry is reported.
 * @param err Where a failure is reported.
 * @return 0 when every entry was written, 1 otherwise.
 *
 * @throws std::runtime_error If the document cannot be read, does not parse, or
 * the platform holds no keys for this unit.
 */
ORES_VAULT_SERVICE_EXPORT int provision_from(const ores::database::context& ctx,
                                             const std::string& source,
                                             std::ostream& out,
                                             std::ostream& err);

}

#endif
