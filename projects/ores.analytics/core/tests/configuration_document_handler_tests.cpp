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
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_parameter_generator.hpp"
#include "ores.analytics.api/generators/todays_market_config_generator.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.analytics.core/messaging/configuration_document_handler.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[analytics][configuration_document][handler]");

// Every row is party-scoped and the token carries the test tenant's system
// party, so the handler's reads see what the case wrote.
boost::uuids::uuid system_party(ores::testing::scoped_database_helper& h) {
    for (const auto& p : ores::refdata::repository::party_repository().read_latest(h.context()))
        if (p.tenant_id == h.tenant_id() && p.party_category == "System")
            return p.id;
    throw std::runtime_error("the test tenant has no system party");
}

std::string subject(const std::string& op) {
    return "ores.test.analytics.configuration_document_handler." + op;
}

struct fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;
    boost::uuids::uuid party;

    ores::analytics::messaging::configuration_document_handler handler() {
        return ores::analytics::messaging::configuration_document_handler(
            nats, db.context(), std::optional(keys.verifier()));
    }

    void subscribe(const std::string& op, std::function<void(ores::nats::message)> f) {
        subs.push_back(nats.queue_subscribe(subject(op), "ores.test", std::move(f)));
    }

    std::string token(const std::vector<std::string>& permissions) {
        return keys.token(db.tenant_id().to_string(), party, permissions);
    }

    fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        party = system_party(db);
        subscribe("save_pricing_engines_document", [this](ores::nats::message m) {
            handler().save_pricing_engines_document(std::move(m));
        });
        subscribe("get_pricing_engines_document", [this](ores::nats::message m) {
            handler().get_pricing_engines_document(std::move(m));
        });
        subscribe("delete_pricing_engines_document", [this](ores::nats::message m) {
            handler().delete_pricing_engines_document(std::move(m));
        });
        subscribe("save_todays_market_document", [this](ores::nats::message m) {
            handler().save_todays_market_document(std::move(m));
        });
        subscribe("get_todays_market_document", [this](ores::nats::message m) {
            handler().get_todays_market_document(std::move(m));
        });
        subscribe("delete_todays_market_document", [this](ores::nats::message m) {
            handler().delete_todays_market_document(std::move(m));
        });
    }
};

}

using namespace ores::analytics::messaging;
using ores::testing::decode_reply;
using ores::testing::error_of;
using ores::testing::request;

namespace {

const std::vector<std::string> save_permissions = {"analytics::pricing_model_configs:read",
                                                   "analytics::pricing_model_configs:write"};

/**
 * A document ORE can run: a named configuration, one product that names its
 * pair, and one setting of that product that carries a value.
 *
 * The name carries the configuration's id because a name is unique within the
 * party that owns the document and the rows a run leaves behind stay in the
 * test tenant. Two cases of one run, or two runs, would otherwise collide on
 * the name and be refused for the wrong reason.
 */
pricing_engines_document valid_document(ores::utility::generation::generation_context& gen) {
    pricing_engines_document doc;
    doc.config = ores::analytics::generators::generate_synthetic_pricing_model_config(gen);
    doc.config.name = "ACME end of day pricing " + boost::uuids::to_string(doc.config.id);
    doc.config.change_reason_code = "system.test";
    doc.config.configuration_id = boost::uuids::random_generator()();

    auto product = ores::analytics::generators::generate_synthetic_pricing_model_product(gen);
    product.pricing_model_config_id = doc.config.id;
    product.pricing_engine_type_code = "Swap";
    product.model = "DiscountedCashflows";
    product.engine = "DiscountingSwapEngine";
    product.position = 0;
    product.change_reason_code = "system.test";
    doc.products = {product};

    auto parameter =
        ores::analytics::generators::generate_synthetic_pricing_model_product_parameter(gen);
    parameter.pricing_model_config_id = doc.config.id;
    parameter.pricing_model_product_id = product.id;
    parameter.parameter_scope = "model";
    parameter.parameter_name = "Tolerance";
    parameter.parameter_value = "0.0001";
    parameter.position = 0;
    parameter.change_reason_code = "system.test";
    doc.parameters = {parameter};
    return doc;
}

save_pricing_engines_document_response
save_document(fixture& f, const std::string& token, const pricing_engines_document& doc) {
    const auto reply = decode_reply<save_pricing_engines_document_response>(
        request(f.nats,
                subject("save_pricing_engines_document"),
                token,
                save_pricing_engines_document_request{.document = doc}));
    REQUIRE(reply);
    INFO(reply->message);
    return *reply;
}

/** Whether a document fills the configuration it names, as a read finds it. */
bool stored(fixture& f, const std::string& token, const pricing_engines_document& doc) {
    const auto reply = decode_reply<get_pricing_engines_document_response>(
        request(f.nats,
                subject("get_pricing_engines_document"),
                token,
                get_pricing_engines_document_request{
                    .configuration_id = boost::uuids::to_string(doc.config.configuration_id)}));
    REQUIRE(reply);
    return reply->success;
}

std::vector<std::string> codes_of(const std::vector<pricing_engines_document_problem>& problems) {
    std::vector<std::string> codes;
    for (const auto& problem : problems)
        codes.push_back(problem.code);
    return codes;
}

}

TEST_CASE("a pricing engines document saves, reads back and deletes by its configuration", tags) {
    fixture f;
    const auto token = f.token({"analytics::pricing_model_configs:read",
                                "analytics::pricing_model_configs:write",
                                "analytics::pricing_model_configs:delete"});
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    const auto configuration = boost::uuids::to_string(doc.config.configuration_id);

    const auto saved = decode_reply<save_pricing_engines_document_response>(
        request(f.nats,
                subject("save_pricing_engines_document"),
                token,
                save_pricing_engines_document_request{.document = doc}));
    REQUIRE(saved);
    INFO(saved->message);
    REQUIRE(saved->success);

    const auto got = decode_reply<get_pricing_engines_document_response>(
        request(f.nats,
                subject("get_pricing_engines_document"),
                token,
                get_pricing_engines_document_request{.configuration_id = configuration}));
    REQUIRE(got);
    REQUIRE(got->success);
    CHECK(got->document.config.id == doc.config.id);
    CHECK(got->document.config.party_id == f.party);

    const auto deleted = decode_reply<delete_pricing_engines_document_response>(
        request(f.nats,
                subject("delete_pricing_engines_document"),
                token,
                delete_pricing_engines_document_request{.configuration_id = configuration}));
    REQUIRE(deleted);
    CHECK(deleted->success);
    const auto gone = decode_reply<get_pricing_engines_document_response>(
        request(f.nats,
                subject("get_pricing_engines_document"),
                token,
                get_pricing_engines_document_request{.configuration_id = configuration}));
    REQUIRE(gone);
    CHECK_FALSE(gone->success);
}

TEST_CASE("saving a pricing engines document again removes what it omits", tags) {
    fixture f;
    const auto token = f.token(
        {"analytics::pricing_model_configs:read", "analytics::pricing_model_configs:write"});
    auto gen = ores::testing::make_generation_context(f.db);
    ores::analytics::messaging::pricing_engines_document doc;
    doc.config = ores::analytics::generators::generate_synthetic_pricing_model_config(gen);
    doc.config.change_reason_code = "system.test";
    doc.config.configuration_id = boost::uuids::random_generator()();
    const auto configuration = boost::uuids::to_string(doc.config.configuration_id);

    const auto product = [&](const std::string& type, int position) {
        auto p = ores::analytics::generators::generate_synthetic_pricing_model_product(gen);
        p.pricing_model_config_id = doc.config.id;
        p.pricing_engine_type_code = type;
        p.position = position;
        p.change_reason_code = "system.test";
        return p;
    };
    doc.products = {product("Swap", 0), product("CapFloor", 1)};
    auto parameter =
        ores::analytics::generators::generate_synthetic_pricing_model_product_parameter(gen);
    parameter.pricing_model_config_id = doc.config.id;
    parameter.pricing_model_product_id = doc.products.front().id;
    parameter.change_reason_code = "system.test";
    doc.parameters = {parameter};

    const auto save = [&](const ores::analytics::messaging::pricing_engines_document& d) {
        const auto reply = decode_reply<save_pricing_engines_document_response>(
            request(f.nats,
                    subject("save_pricing_engines_document"),
                    token,
                    save_pricing_engines_document_request{.document = d}));
        REQUIRE(reply);
        INFO(reply->message);
        REQUIRE(reply->success);
    };
    const auto read = [&] {
        const auto reply = decode_reply<get_pricing_engines_document_response>(
            request(f.nats,
                    subject("get_pricing_engines_document"),
                    token,
                    get_pricing_engines_document_request{.configuration_id = configuration}));
        REQUIRE(reply);
        REQUIRE(reply->success);
        return reply->document;
    };

    save(doc);
    auto held = read();
    REQUIRE(held.products.size() == 2);
    REQUIRE(held.parameters.size() == 1);

    // The second save keeps the first product and omits the second product and the parameter.
    auto next = held;
    std::erase_if(next.products,
                  [](const auto& p) { return p.pricing_engine_type_code == "CapFloor"; });
    next.parameters.clear();
    save(next);

    const auto after = read();
    REQUIRE(after.products.size() == 1);
    CHECK(after.products.front().pricing_engine_type_code == "Swap");
    CHECK(after.parameters.empty());
}

TEST_CASE("a today's market document saves and reads back by its configuration", tags) {
    fixture f;
    const auto token = f.token(
        {"analytics::todays_market_configs:read", "analytics::todays_market_configs:write"});
    auto gen = ores::testing::make_generation_context(f.db);
    ores::analytics::messaging::todays_market_document doc;
    doc.config = ores::analytics::generators::generate_synthetic_todays_market_config(gen);
    doc.config.change_reason_code = "system.test";
    doc.config.configuration_id = boost::uuids::random_generator()();

    const auto saved = decode_reply<save_todays_market_document_response>(
        request(f.nats,
                subject("save_todays_market_document"),
                token,
                save_todays_market_document_request{.document = doc}));
    REQUIRE(saved);
    INFO(saved->message);
    REQUIRE(saved->success);

    const auto got = decode_reply<get_todays_market_document_response>(
        request(f.nats,
                subject("get_todays_market_document"),
                token,
                get_todays_market_document_request{
                    .configuration_id = boost::uuids::to_string(doc.config.configuration_id)}));
    REQUIRE(got);
    REQUIRE(got->success);
    CHECK(got->document.config.id == doc.config.id);
}

TEST_CASE("a caller without the permission is refused", tags) {
    fixture f;
    const auto token = f.token({"analytics::pricing_model_configs:read"});
    const auto reply = request(f.nats,
                               subject("save_pricing_engines_document"),
                               token,
                               save_pricing_engines_document_request{});
    CHECK(error_of(reply) == "forbidden");
}

TEST_CASE("a valid document still saves", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);

    const auto saved = save_document(f, token, doc);
    CHECK(saved.problems.empty());
    REQUIRE(saved.success);
    CHECK(stored(f, token, doc));
}

TEST_CASE("a save with no name is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.config.name = "  ";

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    CHECK(saved.message == "The document has 1 problem.");
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"name_missing"});
    CHECK(saved.problems.front().product_id.empty());
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("a save with no products is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.products.clear();
    doc.parameters.clear();

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"no_products"});
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("a product with no model is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.products.front().model = "";

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"model_missing"});
    CHECK(saved.problems.front().product_id == boost::uuids::to_string(doc.products.front().id));
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("a product with no engine is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.products.front().engine = "";

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"engine_missing"});
    CHECK(saved.problems.front().product_id == boost::uuids::to_string(doc.products.front().id));
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("a product that states the same setting twice is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    auto repeat = doc.parameters.front();
    repeat.id = boost::uuids::random_generator()();
    repeat.position = 1;
    doc.parameters.push_back(repeat);

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"parameter_repeated"});
    const auto& problem = saved.problems.front();
    CHECK(problem.product_id == boost::uuids::to_string(doc.products.front().id));
    CHECK(problem.parameter_scope == "model");
    CHECK(problem.parameter_name == "Tolerance");
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("a setting with no value is refused", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.parameters.front().parameter_value = "";

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    CHECK(saved.message == "The document has 1 problem.");
    REQUIRE(codes_of(saved.problems) == std::vector<std::string>{"parameter_value_missing"});
    const auto& problem = saved.problems.front();
    CHECK(problem.product_id == boost::uuids::to_string(doc.products.front().id));
    CHECK(problem.parameter_scope == "model");
    CHECK(problem.parameter_name == "Tolerance");
    CHECK_FALSE(stored(f, token, doc));
}

TEST_CASE("every problem of a save is refused at once", tags) {
    fixture f;
    const auto token = f.token(save_permissions);
    auto gen = ores::testing::make_generation_context(f.db);
    auto doc = valid_document(gen);
    doc.config.name = "";
    doc.products.front().engine = "";
    doc.parameters.front().parameter_value = "";
    auto repeat = doc.parameters.front();
    repeat.id = boost::uuids::random_generator()();
    repeat.position = 1;
    repeat.parameter_value = "0.0002";
    doc.parameters.push_back(repeat);

    const auto saved = save_document(f, token, doc);
    CHECK_FALSE(saved.success);
    CHECK(saved.message == "The document has 4 problems.");
    REQUIRE(codes_of(saved.problems) ==
            std::vector<std::string>{
                "name_missing", "engine_missing", "parameter_value_missing", "parameter_repeated"});
    CHECK_FALSE(stored(f, token, doc));
}
