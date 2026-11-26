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
#include "ores.vault.core/service/custody_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.security/crypto/secret_sealer.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.vault.api/domain/secret.hpp"
#include "ores.vault.api/domain/secret_use.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "ores.vault.core/repository/consumer_repository.hpp"
#include "ores.vault.core/repository/secret_repository.hpp"
#include "ores.vault.core/repository/secret_use_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <exception>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::vault::service {

using namespace ores::logging;
using ores::security::crypto::sealed_box;
using ores::security::crypto::secret_sealer;
using ores::utility::convert::base64_converter;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.vault.service.custody_service");
    return instance;
}

/// The outcome a write records. The record's whole point is that an act says
/// what it was, so the words are constants.
constexpr std::string_view written = "written";
constexpr std::string_view resolved = "resolved";
constexpr std::string_view removed = "deleted";
constexpr std::string_view rotated = "rotated";
constexpr std::string_view refused_unknown_secret = "refused_unknown_secret";
constexpr std::string_view refused_not_owner = "refused_not_owner";
constexpr std::string_view refused_no_consumer_key = "refused_no_consumer_key";

}

custody_service::custody_service(const ores::database::context& ctx, security::key_material keys)
    : ctx_(ctx)
    , keys_(std::move(keys)) {}

std::string custody_service::public_key() const {
    return base64_converter::convert(keys_.transport_public_key);
}

std::string custody_service::row_context(const std::string& name,
                                         const std::string& owner,
                                         int key_version) const {
    return ctx_.tenant_id().to_string() + "|" + name + "|" + owner + "|" +
           std::to_string(key_version);
}

custody_result custody_service::write(const std::string& actor,
                                      const std::string& name,
                                      const std::string& owner,
                                      const std::string& sealed_to_vault) {
    custody_result answer{.name = name};

    if (name.empty() || owner.empty()) {
        answer.message = "A secret needs a name and an owner";
        return answer;
    }

    try {
        // The value the caller sent is sealed to this process, and only this
        // process can open it: a broker, a log or a trace holds the ciphertext.
        const std::string value = sealed_box::open(keys_.transport_private_key,
                                                   sealed_to_vault,
                                                   messaging::custody_context::write(name, owner));

        const auto version = keys_.current_version;
        const secret_sealer sealer(keys_.current_key());
        const auto sealed = sealer.seal(value, row_context(name, owner, version));

        repository::secret_repository repo;
        const auto existing = repo.read_latest_by_name(ctx_, name);
        domain::secret row;
        if (!existing.empty()) {
            row = existing.front();
        } else {
            boost::uuids::random_generator generate;
            row.id = generate();
            row.name = name;
        }
        row.tenant_id = ctx_.tenant_id();
        row.owner = owner;
        row.sealed_value = sealed;
        row.key_version = version;
        row.modified_by = actor;
        row.performed_by = actor;
        repo.write(ctx_, row);

        record_use(actor, name, std::string(written), version);

        ORES_LOG_SEV(lg(), info) << "Wrote secret " << name << " at key version " << version;
        answer.success = true;
        answer.key_version = version;
        return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Write of " << name << " failed: " << e.what();
        answer.message = e.what();
        return answer;
    }
}

custody_result custody_service::register_consumer(const std::string& subject,
                                                  const std::string& public_key) {
    custody_result answer;

    if (subject.empty() || public_key.empty()) {
        answer.message = "A consumer needs a subject and a public key";
        return answer;
    }

    try {
        const auto raw = base64_converter::convert(public_key);
        if (raw.size() != sealed_box::KEY_LEN) {
            answer.message = "A consumer's public key is 32 bytes";
            return answer;
        }

        repository::consumer_repository consumers;
        const auto existing = consumers.read_latest_by_name(ctx_, subject);

        domain::consumer row;
        if (!existing.empty()) {
            row = existing.front();
        } else {
            boost::uuids::random_generator generate;
            row.id = generate();
            row.name = subject;
        }
        row.tenant_id = ctx_.tenant_id();
        row.public_key = public_key;
        row.is_enabled = true;
        row.modified_by = subject;
        row.performed_by = subject;
        consumers.write(ctx_, row);

        answer.success = true;
        answer.subject = subject;
        ORES_LOG_SEV(lg(), info) << "Registered consumer " << subject;
        return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Registration of " << subject << " failed: " << e.what();
        answer.message = e.what();
        return answer;
    }
}

custody_result custody_service::describe(const std::string& name) const {
    custody_result answer{.name = name};

    repository::secret_repository repo;
    const auto rows = repo.read_latest_by_name(ctx_, name);
    if (rows.empty()) {
        answer.message = "No such secret in this tenant";
        return answer;
    }

    const auto& row = rows.front();
    answer.success = true;
    answer.owner = row.owner;
    answer.key_version = row.key_version;
    answer.written_at = row.recorded_at;
    return answer;
}

custody_result custody_service::resolve(const std::string& subject, const std::string& name) {
    custody_result answer{.name = name};

    repository::secret_repository repo;
    const auto rows = repo.read_latest_by_name(ctx_, name);
    if (rows.empty()) {
        record_use(subject, name, std::string(refused_unknown_secret), 0);
        answer.message = "No such secret in this tenant";
        return answer;
    }

    const auto& row = rows.front();
    if (row.owner != subject) {
        record_use(subject, name, std::string(refused_not_owner), row.key_version);
        answer.message = "This secret belongs to another consumer";
        return answer;
    }

    repository::consumer_repository consumers;
    const auto consumer_rows = consumers.read_latest_by_name(ctx_, subject);
    if (consumer_rows.empty() || !consumer_rows.front().is_enabled) {
        record_use(subject, name, std::string(refused_no_consumer_key), row.key_version);
        answer.message = "The consumer that asked has no registered key, or is disabled";
        return answer;
    }

    try {
        const secret_sealer sealer(keys_.key_of(row.key_version));
        const std::string value =
            sealer.open(row.sealed_value, row_context(row.name, row.owner, row.key_version));

        const auto delivered = base64_converter::convert(consumer_rows.front().public_key);
        answer.sealed_value =
            sealed_box::seal(std::vector<unsigned char>(delivered.begin(), delivered.end()),
                             value,
                             messaging::custody_context::delivery(subject, name, row.key_version));
        answer.key_version = row.key_version;
        answer.subject = subject;
        answer.success = true;

        record_use(subject, name, std::string(resolved), row.key_version);
        ORES_LOG_SEV(lg(), info) << "Resolved " << name << " to " << subject << " at version "
                                 << row.key_version;
        return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Resolve of " << name << " failed: " << e.what();
        answer.message = e.what();
        return answer;
    }
}

custody_result custody_service::remove(const std::string& actor, const std::string& name) {
    custody_result answer{.name = name};

    repository::secret_repository repo;
    const auto rows = repo.read_latest_by_name(ctx_, name);
    if (rows.empty()) {
        record_use(actor, name, std::string(refused_unknown_secret), 0);
        answer.message = "No such secret in this tenant";
        return answer;
    }

    try {
        repo.remove(ctx_, boost::uuids::to_string(rows.front().id));
        record_use(actor, name, std::string(removed), 0);
        answer.success = true;
        ORES_LOG_SEV(lg(), info) << "Removed secret " << name;
        return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Delete of " << name << " failed: " << e.what();
        answer.message = e.what();
        return answer;
    }
}

custody_result custody_service::rotate(const std::string& actor) {
    custody_result answer;
    const auto version = keys_.current_version;
    answer.key_version = version;

    repository::secret_repository repo;
    secret_sealer current(keys_.current_key());

    try {
        for (const auto& row : repo.read_latest(ctx_)) {
            if (row.key_version == version) {
                continue;
            }

            const secret_sealer previous(keys_.key_of(row.key_version));
            const std::string value =
                previous.open(row.sealed_value, row_context(row.name, row.owner, row.key_version));
            const auto sealed = current.seal(value, row_context(row.name, row.owner, version));

            domain::secret resealed = row;
            resealed.tenant_id = ctx_.tenant_id();
            resealed.sealed_value = sealed;
            resealed.key_version = version;
            resealed.modified_by = actor;
            resealed.performed_by = actor;
            repo.write(ctx_, resealed);

            record_use(actor, row.name, std::string(rotated), version);
            ++answer.rotated;
            ORES_LOG_SEV(lg(), info) << "Re-wrapped " << row.name << " from version "
                                     << row.key_version << " to " << version;
        }

        answer.success = true;
        return answer;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Rotation failed: " << e.what();
        answer.message = e.what();
        return answer;
    }
}

void custody_service::record_use(const std::string& subject,
                                 const std::string& name,
                                 const std::string& outcome,
                                 std::int32_t key_version) const {
    repository::secret_use_repository repo;

    domain::secret_use use;
    boost::uuids::random_generator generate;
    use.tenant_id = ctx_.tenant_id();
    use.id = generate();
    use.name = name;
    use.subject = subject;
    use.outcome = outcome;
    use.key_version = key_version;
    use.used_at = std::chrono::system_clock::now();

    try {
        repo.write(ctx_, use);
    } catch (const std::exception& e) {
        // An act that cannot be recorded is an act that did not happen: the
        // caller hears about it rather than receiving a value nobody logged.
        ORES_LOG_SEV(lg(), error) << "Could not record the use of " << name << ": " << e.what();
        throw;
    }
}

}
