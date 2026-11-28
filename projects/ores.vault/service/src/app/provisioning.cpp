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
#include "ores.vault.service/app/provisioning.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include "ores.vault.core/service/custody_service.hpp"
#include <boost/json.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace ores::vault::service::app {

namespace {

using namespace ores::logging;

inline static std::string_view logger_name = "ores.vault.service.app.provisioning";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/// One member of an object, refusing an entry that does not carry it.
[[nodiscard]] std::string member(const boost::json::object& object, const std::string_view name) {
    const auto found = object.find(name);
    if (found == object.end() || !found->value().is_string()) {
        throw std::runtime_error("The provisioning document has an entry with no " +
                                 std::string(name));
    }
    return std::string(found->value().as_string());
}

/// The array a document member names, or nothing when it names none.
[[nodiscard]] const boost::json::array* entries(const boost::json::object& document,
                                                const std::string_view name) {
    const auto found = document.find(name);
    if (found == document.end())
        return nullptr;
    if (!found->value().is_array()) {
        throw std::runtime_error("The provisioning document's " + std::string(name) +
                                 " is not a list");
    }
    return &found->value().as_array();
}

[[nodiscard]] std::string read_source(const std::string& source) {
    if (source == "-") {
        std::ostringstream buffer;
        buffer << std::cin.rdbuf();
        return buffer.str();
    }

    std::ifstream in(source, std::ios::binary);
    if (!in)
        throw std::runtime_error("No provisioning document at " + source);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

}

provision_document provision_document::read(const std::string_view text) {
    boost::json::value parsed;
    try {
        parsed = boost::json::parse(text);
    } catch (const std::exception& e) {
        throw std::runtime_error("The provisioning document does not parse: " +
                                 std::string(e.what()));
    }
    if (!parsed.is_object())
        throw std::runtime_error("The provisioning document is not an object");
    const auto& document = parsed.as_object();

    provision_document answer;
    if (const auto* consumers = entries(document, "consumers")) {
        for (const auto& entry : *consumers) {
            if (!entry.is_object())
                throw std::runtime_error("A consumer entry is not an object");
            const auto& object = entry.as_object();
            answer.consumers.push_back(
                provisioned_consumer{.name = member(object, "name"),
                                     .actor = member(object, "actor"),
                                     .public_key = member(object, "public_key")});
        }
    }
    if (const auto* secrets = entries(document, "secrets")) {
        for (const auto& entry : *secrets) {
            if (!entry.is_object())
                throw std::runtime_error("A secret entry is not an object");
            const auto& object = entry.as_object();
            answer.secrets.push_back(provisioned_secret{.name = member(object, "name"),
                                                        .owner = member(object, "owner"),
                                                        .value = member(object, "value")});
        }
    }
    return answer;
}

int provision_from(const ores::database::context& ctx,
                   const std::string& source,
                   std::ostream& out,
                   std::ostream& err) {
    const auto document = provision_document::read(read_source(source));
    const auto keys = security::credential_source::load_from_credentials_directory();
    service::custody_service custody(ctx, keys);

    ORES_LOG_SEV(lg(), info) << "Provisioning " << document.consumers.size() << " consumers and "
                             << document.secrets.size() << " secrets as " << ctx.actor();

    int failed = 0;
    const auto report = [&](const std::string_view kind,
                            const std::string& name,
                            const service::custody_result& answer) {
        if (!answer.success) {
            err << kind << " " << name << ": " << answer.message << std::endl;
            ++failed;
            return;
        }
        out << kind << " " << name;
        if (answer.key_version != 0)
            out << " at version " << answer.key_version;
        out << std::endl;
    };

    for (const auto& consumer : document.consumers)
        report("consumer",
               consumer.name,
               custody.provision_consumer(consumer.name, consumer.actor, consumer.public_key));

    for (const auto& secret : document.secrets)
        report("secret",
               secret.name,
               custody.provision_secret(ctx.actor(), secret.name, secret.owner, secret.value));

    ORES_LOG_SEV(lg(), info) << "Provisioning wrote "
                             << document.consumers.size() + document.secrets.size() -
                                    static_cast<std::size_t>(failed)
                             << " entries and failed " << failed;
    return failed == 0 ? 0 : 1;
}

}
