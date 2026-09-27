#include "ores.shell/app/commands/dq/dq_commands.hpp"
#include "ores.shell/app/commands/dq/artefact_type_commands.hpp"
#include "ores.shell/app/commands/dq/badge_definition_commands.hpp"
#include "ores.shell/app/commands/dq/catalog_commands.hpp"
#include "ores.shell/app/commands/dq/change_reason_category_commands.hpp"
#include "ores.shell/app/commands/dq/change_reason_commands.hpp"
#include "ores.shell/app/commands/dq/code_domain_commands.hpp"
#include "ores.shell/app/commands/dq/coding_scheme_authority_type_commands.hpp"
#include "ores.shell/app/commands/dq/coding_scheme_commands.hpp"
#include "ores.shell/app/commands/dq/data_domain_commands.hpp"
#include "ores.shell/app/commands/dq/dataset_bundle_commands.hpp"
#include "ores.shell/app/commands/dq/dataset_bundle_member_commands.hpp"
#include "ores.shell/app/commands/dq/fsm_state_commands.hpp"
#include "ores.shell/app/commands/dq/fsm_transition_commands.hpp"
#include "ores.shell/app/commands/dq/lei_entity_commands.hpp"
#include "ores.shell/app/commands/dq/lei_relationship_commands.hpp"
#include "ores.shell/app/commands/dq/methodology_commands.hpp"
#include "ores.shell/app/commands/dq/nature_dimension_commands.hpp"
#include "ores.shell/app/commands/dq/origin_dimension_commands.hpp"
#include "ores.shell/app/commands/dq/report_definition_commands.hpp"
#include "ores.shell/app/commands/dq/synthetic_fx_spot_config_commands.hpp"
#include "ores.shell/app/commands/dq/treatment_dimension_commands.hpp"

namespace ores::shell::app::commands {

void dq_commands::register_commands(cli::Menu& root_menu,
                                    ores::nats::service::nats_client& session) {
    artefact_type_commands::register_commands(root_menu, session);
    badge_definition_commands::register_commands(root_menu, session);
    catalog_commands::register_commands(root_menu, session);
    change_reason_category_commands::register_commands(root_menu, session);
    change_reason_commands::register_commands(root_menu, session);
    coding_scheme_authority_type_commands::register_commands(root_menu, session);
    coding_scheme_commands::register_commands(root_menu, session);
    code_domain_commands::register_commands(root_menu, session);
    data_domain_commands::register_commands(root_menu, session);
    fsm_state_commands::register_commands(root_menu, session);
    fsm_transition_commands::register_commands(root_menu, session);
    dataset_bundle_commands::register_commands(root_menu, session);
    dataset_bundle_member_commands::register_commands(root_menu, session);
    lei_entity_commands::register_commands(root_menu, session);
    lei_relationship_commands::register_commands(root_menu, session);
    methodology_commands::register_commands(root_menu, session);
    nature_dimension_commands::register_commands(root_menu, session);
    origin_dimension_commands::register_commands(root_menu, session);
    report_definition_commands::register_commands(root_menu, session);
    synthetic_fx_spot_config_commands::register_commands(root_menu, session);
    treatment_dimension_commands::register_commands(root_menu, session);
}

}
