#include <imgui.h>
#include "ores.ui/currency_table.hpp"

namespace ores::ui {

void currency_table::display(const ores::core::types::currency_config& ccy_cfg) {

    // Start the table with 10 columns
    if (ImGui::BeginTable("Currencies", 10, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg)) {
        // Set up the columns
        ImGui::TableSetupColumn("Name");
        ImGui::TableSetupColumn("ISO Code");
        ImGui::TableSetupColumn("Numeric Code");
        ImGui::TableSetupColumn("Symbol");
        ImGui::TableSetupColumn("Fraction Symbol");
        ImGui::TableSetupColumn("Fractions Per Unit");
        ImGui::TableSetupColumn("Rounding Type");
        ImGui::TableSetupColumn("Rounding Precision");
        ImGui::TableSetupColumn("Format");
        ImGui::TableSetupColumn("Currency Type");
        ImGui::TableHeadersRow();

        // Iterate over the currencies and display each one in a row
        for (const auto& currency : ccy_cfg.currencies()) {
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::Text("%s", currency.name().c_str());
            ImGui::TableSetColumnIndex(1);
            ImGui::Text("%s", currency.iso_code().c_str());
            ImGui::TableSetColumnIndex(2);
            ImGui::Text("%d", currency.numeric_code());
            ImGui::TableSetColumnIndex(3);
            ImGui::Text("%s", currency.symbol().c_str());
            ImGui::TableSetColumnIndex(4);
            ImGui::Text("%s", currency.fraction_symbol().c_str());
            ImGui::TableSetColumnIndex(5);
            ImGui::Text("%d", currency.fractions_per_unit());
            ImGui::TableSetColumnIndex(6);
            ImGui::Text("%s", currency.rounding_type().c_str());
            ImGui::TableSetColumnIndex(7);
            ImGui::Text("%d", currency.rounding_precision());
            ImGui::TableSetColumnIndex(8);
            ImGui::Text("%s", currency.format().c_str());
            ImGui::TableSetColumnIndex(9);
            ImGui::Text("%s", currency.currency_type().c_str());
        }

        // End the table
        ImGui::EndTable();
    }
}

}
