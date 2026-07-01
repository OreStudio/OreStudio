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
#ifndef ORES_QT_SYNTHETIC_BINDING_DIALOG_HPP
#define ORES_QT_SYNTHETIC_BINDING_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include <QDialog>
#include <QPushButton>
#include <QTableWidget>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Dialog that lists available synthetic FX configs and creates
 *        feed bindings for the ones the user selects.
 *
 * Queries the synthetic service for all fx_spot_generation_config records,
 * presents them in a checklist (ORE key + source name), and on confirmation
 * bulk-creates save_feed_binding_request messages for each checked row.
 * Rows that already have a binding are pre-ticked and shown as such.
 */
class SyntheticBindingDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic_binding_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SyntheticBindingDialog(
        ClientManager* clientManager,
        const std::string& username,
        const std::vector<std::string>& existingSourceNames,
        QWidget* parent = nullptr);

    ~SyntheticBindingDialog() override = default;

    int bindingsCreated() const { return bindingsCreated_; }

private slots:
    void onCreateClicked();
    void onSelectAllClicked();
    void onSelectNoneClicked();

private:
    void loadConfigs();
    void populateTable(const std::vector<synthetic::domain::fx_spot_generation_config>& configs);
    void createBindings(const std::vector<synthetic::domain::fx_spot_generation_config>& selected);

    ClientManager* clientManager_;
    std::string username_;
    std::vector<std::string> existingSourceNames_;

    QTableWidget* table_;
    QPushButton* createButton_;

    std::vector<synthetic::domain::fx_spot_generation_config> configs_;
    int bindingsCreated_{0};
};

}

#endif
