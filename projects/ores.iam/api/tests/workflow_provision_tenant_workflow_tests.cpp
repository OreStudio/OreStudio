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
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace {

const std::string tags("[workflow]");

const std::string tenant_id("11111111-1111-4111-8111-111111111111");
// The tenant the run provisions, which is not the run's own: a run belongs to
// the tenant that asked for it, so the two are stated apart and the steps are
// asserted to act on this one.
const std::string provisioned_tenant_id("44444444-4444-4444-8444-444444444444");
const std::string admin_account_id("22222222-2222-4222-8222-222222222222");
const std::string correlation_id("33333333-3333-4333-8333-333333333333");

using ores::iam::workflow::complete_provisioning_step_kind;
using ores::iam::workflow::detail::unique_step_name;
using ores::iam::workflow::is_declared_step_kind;
using ores::iam::workflow::is_executed_step_kind;
using ores::iam::workflow::is_system_scoped_step_kind;
using ores::iam::workflow::provision_executed_step_kinds;
using ores::iam::workflow::provision_party_step_kind;
using ores::iam::workflow::provision_party_workflow_type;
using ores::iam::workflow::provision_step_kinds;
using ores::iam::workflow::provision_tenant_step;
using ores::iam::workflow::provision_tenant_step_command;
using ores::iam::workflow::provision_tenant_step_subject;
using ores::iam::workflow::provision_tenant_workflow_request;
using ores::iam::workflow::provision_tenant_workflow_type;
using ores::iam::workflow::register_provision_party_workflow;
using ores::iam::workflow::register_provision_tenant_workflow;
using ores::iam::workflow::register_tenant_setup_workflow;
using ores::iam::workflow::start_tenant_setup_step_kind;
using ores::iam::workflow::system_provision_step_kind;
using ores::iam::workflow::tenant_setup_workflow_type;
using ores::workflow::service::workflow_definition;
using ores::workflow::service::workflow_registry;

provision_tenant_step declared(std::string kind, std::string arguments_json = "{}") {
    provision_tenant_step step;
    step.kind = std::move(kind);
    step.arguments_json = std::move(arguments_json);
    return step;
}

std::string request_json(const std::vector<provision_tenant_step>& steps) {
    provision_tenant_workflow_request request;
    request.profile_code = "acme_demo";
    request.tenant_id = provisioned_tenant_id;
    request.tenant_code = "acme";
    request.tenant_hostname = "acme.example";
    request.admin_account_id = admin_account_id;
    request.parameters.push_back(ores::iam::workflow::provision_tenant_parameter{
        .name = "root_lei", .value = "529900T8BM49AURSDO55"});
    request.steps = steps;
    return rfl::json::write(request);
}

workflow_definition definition() {
    workflow_registry registry;
    register_provision_tenant_workflow(registry);

    const auto* found = registry.find(std::string(provision_tenant_workflow_type));
    if (found == nullptr)
        throw std::runtime_error("the definition did not register under its type name");
    return *found;
}

workflow_definition setup_definition() {
    workflow_registry registry;
    register_tenant_setup_workflow(registry);

    const auto* found = registry.find(std::string(tenant_setup_workflow_type));
    if (found == nullptr)
        throw std::runtime_error("the setup definition did not register under its type name");
    return *found;
}

}

TEST_CASE("the system definition registers under its workflow type", tags) {
    workflow_registry registry;
    register_provision_tenant_workflow(registry);

    const auto* found = registry.find(std::string(provision_tenant_workflow_type));
    REQUIRE(found != nullptr);
    CHECK_FALSE(found->description.empty());
}

TEST_CASE("the setup definition registers under its workflow type", tags) {
    workflow_registry registry;
    register_tenant_setup_workflow(registry);

    const auto* found = registry.find(std::string(tenant_setup_workflow_type));
    REQUIRE(found != nullptr);
    CHECK_FALSE(found->description.empty());
}

TEST_CASE("the catalogue knows every declared kind", tags) {
    for (const auto kind : provision_step_kinds)
        CHECK(is_declared_step_kind(kind));
}

TEST_CASE("the catalogue refuses the steps a run appends and an unknown kind", tags) {
    CHECK_FALSE(is_declared_step_kind(complete_provisioning_step_kind));
    CHECK_FALSE(is_declared_step_kind(start_tenant_setup_step_kind));
    CHECK_FALSE(is_declared_step_kind("publish_everything"));
}

TEST_CASE("only the system step is system-scoped", tags) {
    for (const auto kind : provision_step_kinds)
        CHECK(is_system_scoped_step_kind(kind) == (kind == system_provision_step_kind));

    CHECK(is_system_scoped_step_kind(system_provision_step_kind));
    CHECK_FALSE(is_system_scoped_step_kind(start_tenant_setup_step_kind));
    CHECK_FALSE(is_system_scoped_step_kind(complete_provisioning_step_kind));
}

TEST_CASE("a step name is made unique within its run", tags) {
    std::unordered_map<std::string, int> seen;

    CHECK(unique_step_name("provision_party", seen) == "provision_party");
    CHECK(unique_step_name("provision_party", seen) == "provision_party_2");
    CHECK(unique_step_name("publish_bundle", seen) == "publish_bundle");
    CHECK(unique_step_name("provision_party", seen) == "provision_party_3");
}

TEST_CASE("the system run takes the system-scoped kinds and the handing step", tags) {
    const auto def = definition();
    const auto steps =
        def.build_steps(request_json({declared("publish_bundle", R"({"bundles":["acme_group"]})"),
                                      declared("system_provision"),
                                      declared("provision_party")}),
                        tenant_id,
                        correlation_id);

    // The run is the profile's system-scoped kinds alone, in the order the
    // profile declares them, and then the step that hands the tenant over.
    REQUIRE(steps.size() == 2);
    CHECK(steps[0].name == "system_provision");
    CHECK(steps[0].consumes.empty());
    CHECK(steps[1].name == start_tenant_setup_step_kind);
    CHECK(steps[1].consumes == std::vector<std::string>{"system_provision"});

    for (const auto& step : steps) {
        CHECK(step.command_subject == provision_tenant_step_subject);
        CHECK(step.compensation_subject.empty());
        // A screen shows the label and the description, and falls back to the
        // step's identity only when a definition declares none: every kind
        // this definition declares declares both.
        CHECK_FALSE(step.label.empty());
        CHECK_FALSE(step.description.empty());
    }
}

TEST_CASE("the setup run's kinds become steps in order, with the completing step last", tags) {
    const auto def = setup_definition();
    const auto steps =
        def.build_steps(request_json({declared("publish_bundle", R"({"bundles":["acme_group"]})"),
                                      declared("import_lei_hierarchy"),
                                      declared("provision_party")}),
                        tenant_id,
                        correlation_id);

    REQUIRE(steps.size() == 4);
    CHECK(steps[0].name == "publish_bundle");
    CHECK(steps[1].name == "import_lei_hierarchy");
    CHECK(steps[2].name == "provision_party");
    CHECK(steps[3].name == complete_provisioning_step_kind);

    for (const auto& step : steps) {
        CHECK(step.command_subject == provision_tenant_step_subject);
        CHECK(step.compensation_subject.empty());
        // A screen shows the label and the description, and falls back to the
        // step's identity only when a definition declares none: every kind
        // this definition declares declares both.
        CHECK_FALSE(step.label.empty());
        CHECK_FALSE(step.description.empty());
    }
}

TEST_CASE("the setup run's steps declare which steps must answer first", tags) {
    const auto def = setup_definition();
    const auto steps =
        def.build_steps(request_json({declared("system_provision"),
                                      declared("publish_bundle", R"({"bundles":["acme_group"]})"),
                                      declared("import_lei_hierarchy"),
                                      declared("provision_party"),
                                      declared("load_staff"),
                                      declared("attach_photos"),
                                      declared("start_market_feeds")}),
                        tenant_id,
                        correlation_id);

    const auto inputs_of = [&](const std::string& name) {
        for (const auto& step : steps)
            if (step.name == name)
                return step.consumes;
        return std::vector<std::string>{};
    };

    // The system step belongs to the other run, so the first step of this one
    // reads nothing: what it works from was published before this run existed.
    CHECK(inputs_of("publish_bundle").empty());
    CHECK(inputs_of("import_lei_hierarchy") == std::vector<std::string>{"publish_bundle"});
    CHECK(inputs_of("provision_party") == std::vector<std::string>{"import_lei_hierarchy"});
    CHECK(inputs_of("load_staff") == std::vector<std::string>{"provision_party"});
    CHECK(inputs_of("attach_photos") ==
          (std::vector<std::string>{"load_staff", "provision_party"}));
    // The feeds read the reference data and nothing else, which is what lets
    // them run beside the party chain rather than after it.
    CHECK(inputs_of("start_market_feeds") == std::vector<std::string>{"publish_bundle"});
    // Nothing is finished before everything is, so the last step reads them all.
    CHECK(inputs_of(std::string(complete_provisioning_step_kind)).size() == 6);
}

TEST_CASE("a need the run did not order is answered by the step that did", tags) {
    const auto def = setup_definition();
    // A run that orders no import step still waits for whatever published the
    // reference data before it provisions a party from it.
    const auto steps =
        def.build_steps(request_json({declared("publish_bundle"), declared("provision_party")}),
                        tenant_id,
                        correlation_id);

    const auto inputs_of = [&](const std::string& name) {
        for (const auto& step : steps)
            if (step.name == name)
                return step.consumes;
        return std::vector<std::string>{};
    };

    CHECK(inputs_of("publish_bundle").empty());
    CHECK(inputs_of("provision_party") == std::vector<std::string>{"publish_bundle"});
}

TEST_CASE("a step that needs two kinds reads one step when both resolve to it", tags) {
    const auto def = setup_definition();
    // With no staff step, both of the photograph step's needs are met by the
    // party step, and a step reads a step once however many needs found it.
    const auto steps = def.build_steps(request_json({declared("import_lei_hierarchy"),
                                                     declared("provision_party"),
                                                     declared("attach_photos")}),
                                       tenant_id,
                                       correlation_id);

    const auto inputs_of = [&](const std::string& name) {
        for (const auto& step : steps)
            if (step.name == name)
                return step.consumes;
        return std::vector<std::string>{};
    };

    CHECK(inputs_of("attach_photos") == std::vector<std::string>{"provision_party"});
}

TEST_CASE("a profile that orders nothing still runs the step that finishes each run", tags) {
    const auto setup_steps =
        setup_definition().build_steps(request_json({}), tenant_id, correlation_id);
    REQUIRE(setup_steps.size() == 1);
    CHECK(setup_steps.front().name == complete_provisioning_step_kind);

    // The system run has nothing to hand over before it hands over, so the
    // handing step is its only step.
    const auto system_steps =
        definition().build_steps(request_json({}), tenant_id, correlation_id);
    REQUIRE(system_steps.size() == 1);
    CHECK(system_steps.front().name == start_tenant_setup_step_kind);
    CHECK(system_steps.front().consumes.empty());
}

TEST_CASE("a repeated kind yields a distinct step name", tags) {
    const auto def = setup_definition();
    const auto steps =
        def.build_steps(request_json({declared("provision_party"), declared("provision_party")}),
                        tenant_id,
                        correlation_id);

    REQUIRE(steps.size() == 3);
    CHECK(steps[0].name == "provision_party");
    CHECK(steps[1].name == "provision_party_2");
    CHECK(steps[2].name == complete_provisioning_step_kind);
}

TEST_CASE("a step command carries the tenant it provisions, its administrator and the run's "
          "parameters",
          tags) {
    const auto def = setup_definition();
    const auto steps = def.build_steps(
        request_json({declared("provision_party", R"({"party_bundle":"acme_group"})")}),
        tenant_id,
        correlation_id);

    const auto command =
        rfl::json::read<provision_tenant_step_command>(steps[0].build_command("", {}));
    REQUIRE(command);
    CHECK(command->kind == "provision_party");
    // The tenant the step acts on is the one the request names, not the
    // engine's own tenant argument, which is the run's tenant.
    CHECK(command->tenant_id == provisioned_tenant_id);
    CHECK(command->tenant_id != tenant_id);
    CHECK(command->tenant_code == "acme");
    CHECK(command->tenant_hostname == "acme.example");
    CHECK(command->admin_account_id == admin_account_id);
    CHECK(command->arguments_json == R"({"party_bundle":"acme_group"})");
    REQUIRE(command->parameters.size() == 1);
    CHECK(command->parameters[0].name == "root_lei");
    CHECK(command->parameters[0].value == "529900T8BM49AURSDO55");
}

TEST_CASE("the handing step's command carries the whole run request", tags) {
    const auto def = definition();
    const auto request = request_json({declared("system_provision"), declared("provision_party")});
    const auto steps = def.build_steps(request, tenant_id, correlation_id);

    REQUIRE(steps.size() == 2);
    const auto command =
        rfl::json::read<provision_tenant_step_command>(steps[1].build_command("", {}));
    REQUIRE(command);
    CHECK(command->kind == start_tenant_setup_step_kind);
    CHECK(command->tenant_id == provisioned_tenant_id);
    CHECK(command->admin_account_id == admin_account_id);
    // The child works from the same request this run received, so the step's
    // argument is that whole request and not the kind's own shape.
    CHECK(command->arguments_json == request);
}

TEST_CASE("the step list is the same on every call", tags) {
    const auto def = setup_definition();
    const auto request = request_json({declared("publish_bundle"), declared("provision_party")});

    const auto first = def.build_steps(request, tenant_id, correlation_id);
    const auto second = def.build_steps(request, tenant_id, correlation_id);

    REQUIRE(first.size() == second.size());
    for (std::size_t i = 0; i < first.size(); ++i) {
        CHECK(first[i].name == second[i].name);
        CHECK(first[i].command_subject == second[i].command_subject);
        CHECK(first[i].build_command("", {}) == second[i].build_command("", {}));
    }
}

TEST_CASE("a kind the catalogue does not know is refused by either run", tags) {
    CHECK_THROWS_AS(
        definition().build_steps(
            request_json({declared("publish_everything")}), tenant_id, correlation_id),
        std::runtime_error);
    CHECK_THROWS_AS(
        setup_definition().build_steps(
            request_json({declared("publish_everything")}), tenant_id, correlation_id),
        std::runtime_error);
}

TEST_CASE("every kind the catalogue declares has a runner in this build", tags) {
    for (const auto kind : provision_executed_step_kinds)
        CHECK(is_executed_step_kind(kind));

    CHECK(is_executed_step_kind("publish_bundle"));
    CHECK(is_executed_step_kind("import_lei_hierarchy"));
    CHECK(is_executed_step_kind("provision_party"));
    CHECK(is_executed_step_kind("load_staff"));
    CHECK(is_executed_step_kind("attach_photos"));
    CHECK(is_executed_step_kind("start_market_feeds"));

    CHECK_FALSE(is_executed_step_kind(complete_provisioning_step_kind));
    CHECK_FALSE(is_executed_step_kind(start_tenant_setup_step_kind));
    CHECK_FALSE(is_executed_step_kind("publish_everything"));
}

TEST_CASE("the demo card's kinds become steps in the profile's order", tags) {
    const auto def = setup_definition();
    const auto steps = def.build_steps(
        request_json(
            {declared("load_staff",
                      R"({"parties":[{"name":"Acme Corporation Plc","bundles":["acme_group"]}]})"),
             declared(
                 "attach_photos",
                 R"({"parties":[{"name":"Acme Corporation Plc","dataset":"acme.acme_group.accounts"}]})"),
             declared("start_market_feeds", R"({"theme":"synthetic.themes.realistic_2026"})")}),
        tenant_id,
        correlation_id);

    REQUIRE(steps.size() == 4);
    CHECK(steps[0].name == "load_staff");
    CHECK(steps[1].name == "attach_photos");
    CHECK(steps[2].name == "start_market_feeds");
    CHECK(steps[3].name == complete_provisioning_step_kind);
}

TEST_CASE("a profile may not order a step a run appends", tags) {
    const auto def = definition();

    try {
        def.build_steps(request_json({declared(std::string(complete_provisioning_step_kind))}),
                        tenant_id,
                        correlation_id);
        FAIL("the definition accepted the completing step as a declared kind");
    } catch (const std::runtime_error& e) {
        CHECK(std::string(e.what()).find("appends itself") != std::string::npos);
    }

    try {
        def.build_steps(request_json({declared(std::string(start_tenant_setup_step_kind))}),
                        tenant_id,
                        correlation_id);
        FAIL("the definition accepted the handing step as a declared kind");
    } catch (const std::runtime_error& e) {
        CHECK(std::string(e.what()).find("appends itself") != std::string::npos);
    }
}

TEST_CASE("a request this deployment cannot read is refused by either run", tags) {
    CHECK_THROWS_AS(definition().build_steps("not a request", tenant_id, correlation_id),
                    std::runtime_error);
    CHECK_THROWS_AS(setup_definition().build_steps("not a request", tenant_id, correlation_id),
                    std::runtime_error);
}

TEST_CASE("a request that names no tenant to provision is refused by either run", tags) {
    provision_tenant_workflow_request request;
    request.profile_code = "acme_demo";
    request.tenant_code = "acme";
    request.tenant_hostname = "acme.example";
    request.admin_account_id = admin_account_id;
    request.steps = {declared("publish_bundle")};

    for (const auto& def : {definition(), setup_definition()}) {
        try {
            def.build_steps(rfl::json::write(request), tenant_id, correlation_id);
            FAIL("the definition accepted a request that names no tenant to provision");
        } catch (const std::runtime_error& e) {
            CHECK(std::string(e.what()).find("names no tenant to provision") != std::string::npos);
        }
    }
}

namespace {

workflow_definition party_definition() {
    workflow_registry registry;
    register_provision_party_workflow(registry);

    const auto* found = registry.find(std::string(provision_party_workflow_type));
    if (found == nullptr)
        throw std::runtime_error("the party definition did not register under its type name");
    return *found;
}

}

TEST_CASE("the party definition registers under its own workflow type", tags) {
    workflow_registry registry;
    register_provision_party_workflow(registry);

    const auto* found = registry.find(std::string(provision_party_workflow_type));
    REQUIRE(found != nullptr);
    CHECK_FALSE(found->description.empty());
}

TEST_CASE("a party run is its declared steps and nothing that completes the tenant", tags) {
    const auto def = party_definition();
    const auto steps = def.build_steps(
        request_json({declared(std::string(provision_party_step_kind),
                               R"({"bundles":["party_essentials"],"party":"BARCLAYS PLC"})")}),
        tenant_id,
        correlation_id);

    REQUIRE(steps.size() == 1);
    CHECK(steps.front().name == provision_party_step_kind);
    CHECK(steps.front().command_subject == provision_tenant_step_subject);

    const auto command =
        rfl::json::read<provision_tenant_step_command>(steps.front().build_command("", {}));
    REQUIRE(command);
    CHECK(command->arguments_json == R"({"bundles":["party_essentials"],"party":"BARCLAYS PLC"})");
    CHECK(command->admin_account_id == admin_account_id);
}

TEST_CASE("a party run that orders a kind this build cannot execute is refused", tags) {
    const auto def = party_definition();

    CHECK_THROWS_AS(
        def.build_steps(request_json({declared("publish_everything")}), tenant_id, correlation_id),
        std::runtime_error);
    CHECK_THROWS_AS(
        def.build_steps(request_json({declared(std::string(complete_provisioning_step_kind))}),
                        tenant_id,
                        correlation_id),
        std::runtime_error);
}
