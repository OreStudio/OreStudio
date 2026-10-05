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
#include "ores.dq.core/messaging/registrar.hpp"
#include "ores.dq.api/messaging/artefact_type_protocol.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.dq.api/messaging/change_reason_category_protocol.hpp"
#include "ores.dq.api/messaging/change_reason_protocol.hpp"
#include "ores.dq.api/messaging/data_domain_protocol.hpp"
#include "ores.dq.api/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.dq.api/messaging/publication_protocol.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_datasets_protocol.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.dq.core/messaging/artefact_type_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/artefact_type_registrar.hpp"
#include "ores.dq.core/messaging/badge_definition_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/badge_definition_registrar.hpp"
#include "ores.dq.core/messaging/badge_mapping_registrar.hpp"
#include "ores.dq.core/messaging/badge_severity_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/badge_severity_registrar.hpp"
#include "ores.dq.core/messaging/catalog_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/catalog_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_category_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_category_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/change_reason_registrar.hpp"
#include "ores.dq.core/messaging/code_domain_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/code_domain_registrar.hpp"
#include "ores.dq.core/messaging/coding_scheme_authority_type_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/coding_scheme_authority_type_registrar.hpp"
#include "ores.dq.core/messaging/coding_scheme_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/coding_scheme_registrar.hpp"
#include "ores.dq.core/messaging/counterparty_alias_registrar.hpp"
#include "ores.dq.core/messaging/csa_registrar.hpp"
#include "ores.dq.core/messaging/data_domain_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/data_domain_registrar.hpp"
#include "ores.dq.core/messaging/dataset_bundle_handler.hpp"
#include "ores.dq.core/messaging/dataset_bundle_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/dataset_bundle_member_handler.hpp"
#include "ores.dq.core/messaging/dataset_bundle_member_registrar.hpp"
#include "ores.dq.core/messaging/dataset_bundle_registrar.hpp"
#include "ores.dq.core/messaging/dataset_handler.hpp"
#include "ores.dq.core/messaging/dataset_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/dataset_registrar.hpp"
#include "ores.dq.core/messaging/fsm_state_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/fsm_state_registrar.hpp"
#include "ores.dq.core/messaging/fsm_transition_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/fsm_transition_registrar.hpp"
#include "ores.dq.core/messaging/lei_entity_registrar.hpp"
#include "ores.dq.core/messaging/lei_entity_summary_handler.hpp"
#include "ores.dq.core/messaging/lei_relationship_registrar.hpp"
#include "ores.dq.core/messaging/methodology_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/methodology_registrar.hpp"
#include "ores.dq.core/messaging/nature_dimension_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/nature_dimension_registrar.hpp"
#include "ores.dq.core/messaging/netting_agreement_registrar.hpp"
#include "ores.dq.core/messaging/netting_set_alias_registrar.hpp"
#include "ores.dq.core/messaging/netting_set_registrar.hpp"
#include "ores.dq.core/messaging/origin_dimension_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/origin_dimension_registrar.hpp"
#include "ores.dq.core/messaging/publication_registrar.hpp"
#include "ores.dq.core/messaging/publish_from_dq_handler.hpp"
#include "ores.dq.core/messaging/publish_handler.hpp"
#include "ores.dq.core/messaging/report_definition_registrar.hpp"
#include "ores.dq.core/messaging/report_definition_template_handler.hpp"
#include "ores.dq.core/messaging/subject_area_registrar.hpp"
#include "ores.dq.core/messaging/synthetic_fx_spot_config_registrar.hpp"
#include "ores.dq.core/messaging/treatment_dimension_history_provider_registrar.hpp"
#include "ores.dq.core/messaging/treatment_dimension_registrar.hpp"
#include "ores.dq.core/presentation/subject_area_history_field_mapper.hpp"
#include "ores.dq.core/service/subject_area_service.hpp"
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include <array>
#include <memory>
#include <string_view>

namespace ores::dq::messaging {

namespace {

constexpr std::string_view queue_group = "ores.dq.service";

// Function-local static: must outlive the history.v1.get subscription
// (see ores::history::messaging::register_history_handlers's doc), and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // =========================================================================
    // FSM states and transitions are on the standard generated stack; see each
    // entity's own _handler/_registrar pair.
    // =========================================================================

    {
        auto fsm_state_subs = register_fsm_state_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(fsm_state_subs.begin()),
                    std::make_move_iterator(fsm_state_subs.end()));
    }
    {
        auto fsm_transition_subs = register_fsm_transition_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(fsm_transition_subs.begin()),
                    std::make_move_iterator(fsm_transition_subs.end()));
    }

    // =========================================================================
    // Change reason category and change reason are both on the standard
    // generated stack (see change_reason_category_handler/_registrar and
    // change_reason_handler/_registrar).
    // =========================================================================

    {
        auto change_reason_category_subs =
            register_change_reason_category_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(change_reason_category_subs.begin()),
                    std::make_move_iterator(change_reason_category_subs.end()));

        auto change_reason_subs = register_change_reason_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(change_reason_subs.begin()),
                    std::make_move_iterator(change_reason_subs.end()));
    }

    // =========================================================================
    // Catalog and data_domain are on the standard generated stack (see
    // catalog_handler/_registrar and data_domain_handler/_registrar).
    // Methodologies and subject-areas use the bespoke data_organization_handler.
    // =========================================================================

    {
        auto catalog_subs = register_catalog_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(catalog_subs.begin()),
                    std::make_move_iterator(catalog_subs.end()));

        auto data_domain_subs = register_data_domain_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(data_domain_subs.begin()),
                    std::make_move_iterator(data_domain_subs.end()));
    }

    // =========================================================================
    // Artefact type is on the standard generated stack (see
    // artefact_type_handler/_registrar).
    // =========================================================================

    {
        auto artefact_type_subs = register_artefact_type_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(artefact_type_subs.begin()),
                    std::make_move_iterator(artefact_type_subs.end()));
    }

    // =========================================================================
    // Methodologies are on the standard generated stack (see
    // methodology_handler/_registrar).
    // =========================================================================

    {
        auto methodology_subs = register_methodology_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(methodology_subs.begin()),
                    std::make_move_iterator(methodology_subs.end()));
    }

    // =========================================================================
    // Subject areas are on the standard generated stack (see
    // subject_area_handler/_registrar). Their history provider is hand-written,
    // because the entity's history identity is composite.
    // =========================================================================

    {
        auto subject_area_subs = register_subject_area_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(subject_area_subs.begin()),
                    std::make_move_iterator(subject_area_subs.end()));
    }

    // =========================================================================
    // Dimensions are on the standard generated stack; see each entity's own
    // _handler/_registrar pair.
    // =========================================================================

    {
        auto nature_dimension_subs = register_nature_dimension_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(nature_dimension_subs.begin()),
                    std::make_move_iterator(nature_dimension_subs.end()));
    }
    {
        auto origin_dimension_subs = register_origin_dimension_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(origin_dimension_subs.begin()),
                    std::make_move_iterator(origin_dimension_subs.end()));
    }
    {
        auto treatment_dimension_subs = register_treatment_dimension_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(treatment_dimension_subs.begin()),
                    std::make_move_iterator(treatment_dimension_subs.end()));
    }

    // =========================================================================
    // Datasets
    // =========================================================================

    // Datasets are on the standard generated stack (see
    // dataset_handler/_registrar); publish_handler serves the publish verb.

    {
        auto dataset_subs = register_dataset_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(dataset_subs.begin()),
                    std::make_move_iterator(dataset_subs.end()));
    }

    // Dataset bundles and their members are on the standard generated stack;
    // see register_dataset_bundle_handlers below.

    // =========================================================================
    // Publications
    // =========================================================================

    // The publication run log is on the standard generated stack (see
    // publication_handler/_registrar).

    {
        auto publication_subs = register_publication_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(publication_subs.begin()),
                    std::make_move_iterator(publication_subs.end()));
    }

    // =========================================================================
    // Publication verbs. Both resolve their datasets and dispatch the same
    // bundle-publish workflow, so one handler serves them; see
    // publish_handler.hpp. A direct publication names datasets, a bundle
    // publication names a bundle.
    // =========================================================================

    auto pubh = std::make_shared<publish_handler>(nats, ctx, verifier);

    subs.push_back(nats.queue_subscribe(
        publish_datasets_request::nats_subject, queue_group, [pubh](ores::nats::message msg) {
            pubh->publish_datasets(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        publish_bundle_request::nats_subject, queue_group, [pubh](ores::nats::message msg) {
            pubh->publish_bundle(std::move(msg));
        }));

    // =========================================================================
    // Coding scheme authority types and coding schemes are on the standard
    // generated stack; see each entity's own _handler/_registrar pair.
    // =========================================================================

    {
        auto coding_scheme_authority_type_subs =
            register_coding_scheme_authority_type_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(coding_scheme_authority_type_subs.begin()),
                    std::make_move_iterator(coding_scheme_authority_type_subs.end()));
    }
    {
        auto coding_scheme_subs = register_coding_scheme_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(coding_scheme_subs.begin()),
                    std::make_move_iterator(coding_scheme_subs.end()));
    }

    // =========================================================================
    // LEI Entities, LEI Relationships, Report Definitions, Synthetic FX Spot
    // Configs are on the standard generated stack (see their own
    // *_handler/_registrar pairs).
    // =========================================================================

    {
        auto lei_entity_subs = register_lei_entity_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(lei_entity_subs.begin()),
                    std::make_move_iterator(lei_entity_subs.end()));
    }
    {
        auto lei_relationship_subs = register_lei_relationship_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(lei_relationship_subs.begin()),
                    std::make_move_iterator(lei_relationship_subs.end()));
    }
    {
        auto counterparty_alias_subs = register_counterparty_alias_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(counterparty_alias_subs.begin()),
                    std::make_move_iterator(counterparty_alias_subs.end()));
    }
    {
        auto netting_agreement_subs = register_netting_agreement_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(netting_agreement_subs.begin()),
                    std::make_move_iterator(netting_agreement_subs.end()));
    }
    {
        auto netting_set_subs = register_netting_set_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(netting_set_subs.begin()),
                    std::make_move_iterator(netting_set_subs.end()));
    }
    {
        auto csa_subs = register_csa_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(csa_subs.begin()),
                    std::make_move_iterator(csa_subs.end()));
    }
    {
        auto netting_set_alias_subs = register_netting_set_alias_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(netting_set_alias_subs.begin()),
                    std::make_move_iterator(netting_set_alias_subs.end()));
    }
    {
        auto report_definition_subs = register_report_definition_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(report_definition_subs.begin()),
                    std::make_move_iterator(report_definition_subs.end()));
    }
    {
        auto synthetic_fx_spot_config_subs =
            register_synthetic_fx_spot_config_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(synthetic_fx_spot_config_subs.begin()),
                    std::make_move_iterator(synthetic_fx_spot_config_subs.end()));
    }

    // =========================================================================
    // Report Definition Templates (served from DQ artefact tables)
    // =========================================================================

    {
        auto rdt = std::make_shared<report_definition_template_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(list_dq_report_definition_templates_request::nats_subject,
                                 queue_group,
                                 [rdt](ores::nats::message msg) { rdt->list(std::move(msg)); }));
    }

    // =========================================================================
    // LEI Entity Summary (a projection over the LEI artefact tables)
    // =========================================================================

    {
        auto les = std::make_shared<lei_entity_summary_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(get_lei_entities_summary_request::nats_subject,
                                 queue_group,
                                 [les](ores::nats::message msg) { les->summary(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            search_lei_entities_request::nats_subject, queue_group, [les](ores::nats::message msg) {
                les->search(std::move(msg));
            }));
    }

    // =========================================================================
    // Badges: severities, code domains, definitions and the mapping junction
    // are all on the standard generated stack (see badge_definition_handler/
    // badge_severity_handler/code_domain_handler/badge_mapping_handler).
    // =========================================================================

    {
        auto badge_definition_subs = register_badge_definition_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(badge_definition_subs.begin()),
                    std::make_move_iterator(badge_definition_subs.end()));

        auto badge_mapping_subs = register_badge_mapping_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(badge_mapping_subs.begin()),
                    std::make_move_iterator(badge_mapping_subs.end()));

        auto badge_severity_subs = register_badge_severity_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(badge_severity_subs.begin()),
                    std::make_move_iterator(badge_severity_subs.end()));

        auto code_domain_subs = register_code_domain_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(code_domain_subs.begin()),
                    std::make_move_iterator(code_domain_subs.end()));

        auto dataset_bundle_subs = register_dataset_bundle_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(dataset_bundle_subs.begin()),
                    std::make_move_iterator(dataset_bundle_subs.end()));

        auto dataset_bundle_member_subs =
            register_dataset_bundle_member_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(dataset_bundle_member_subs.begin()),
                    std::make_move_iterator(dataset_bundle_member_subs.end()));
    }

    // ----------------------------------------------------------------
    // Generic history.v1.get subject. The registrar resolves each
    // request into a scoped context exactly like every other subject
    // (make_request_context), so a provider sees the same
    // tenant/party/roles/workspace visibility any other handler in
    // this file would.
    // ----------------------------------------------------------------
    {
        auto& hist_registry = history_registry();

        register_artefact_type_history_provider(hist_registry);
        register_badge_definition_history_provider(hist_registry);
        register_badge_severity_history_provider(hist_registry);
        register_catalog_history_provider(hist_registry);
        register_change_reason_category_history_provider(hist_registry);
        register_change_reason_history_provider(hist_registry);
        register_code_domain_history_provider(hist_registry);
        register_coding_scheme_history_provider(hist_registry);
        register_coding_scheme_authority_type_history_provider(hist_registry);
        register_data_domain_history_provider(hist_registry);
        register_dataset_bundle_history_provider(hist_registry);
        register_dataset_history_provider(hist_registry);
        register_fsm_state_history_provider(hist_registry);
        register_fsm_transition_history_provider(hist_registry);
        register_methodology_history_provider(hist_registry);
        register_nature_dimension_history_provider(hist_registry);
        register_origin_dimension_history_provider(hist_registry);
        register_treatment_dimension_history_provider(hist_registry);

        // subject_area keeps a hand-written provider. Its history identity is
        // the composite (name, domain_name) that the Qt client sends as
        // "name|domain_name" (SubjectAreaController::showHistoryWindow), and
        // the generated registrar declines to register one for a compound key.
        // The generated registrar declines to register a provider for a
        // compound key, so this bridge is hand-written.
        hist_registry.register_history_provider(
            "ores.dq.subject_area",
            [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
                service::subject_area_service svc(scoped_ctx);
                const auto sep = entity_id.find('|');
                const auto name = entity_id.substr(0, sep);
                const auto domain_name =
                    sep == std::string::npos ? std::string{} : entity_id.substr(sep + 1);
                auto versions = svc.get_area_history(name, domain_name);
                return ores::history::service::build_entity_history_versions(
                    versions, presentation::render_subject_area_fields);
            });

        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "dq", queue_group, ctx, verifier));
    }

    // =========================================================================
    // DQ-internal Publish-from-DQ workflow step handlers
    // =========================================================================

    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        static constexpr std::array<std::string_view, 6> subjects = {
            "dq.v1.ip2country.publish-from-dq",
            "dq.v1.coding-schemes.publish-from-dq",
            "dq.v1.badge-severities.publish-from-dq",
            "dq.v1.badge-definitions.publish-from-dq",
            "dq.v1.code-domains.publish-from-dq",
            "dq.v1.badge-mappings.publish-from-dq"};
        for (const auto subject : subjects)
            subs.push_back(
                nats.queue_subscribe(subject, queue_group, [pdq](ores::nats::message msg) {
                    pdq->handle(std::move(msg));
                }));
    }

    return subs;
}

}
