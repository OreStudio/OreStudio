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
#ifndef ORES_QT_BOOTSTRAP_CONFIG_PICKER_DIALOG_HPP
#define ORES_QT_BOOTSTRAP_CONFIG_PICKER_DIALOG_HPP

#include "ores.qt/RefdataExport.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include <QDialog>
#include <optional>
#include <vector>

class QTableWidget;
class QPushButton;
class QLabel;

namespace ores::qt {

class ClientManager;

/**
 * @brief Modal picker over existing ir_curve_bootstrap_config rows -- used by
 * CurveBuilderWorkbench both for a PROJECTION config's Discount Curve Config Id (roleFilter
 * "FUNDING") and for cloning any existing config's Conventions/Pillars into a new one
 * (roleFilter empty -- any role) -- instead of a hand-typed UUID. Read-only: unlike
 * MarketSeriesPickerDialog there is no inline "create new" here, since a config must already
 * exist before another config can reference or clone it.
 */
class ORES_QT_REFDATA_EXPORT BootstrapConfigPickerDialog : public QDialog {
    Q_OBJECT

public:
    /// @param currencyFilter When non-empty (e.g. "USD"), only shows configs whose output series
    /// qualifier contains this currency -- cross-currency discounting is the unusual case, not
    /// the default, so this keeps a novice from picking a mismatched discount curve.
    /// @param roleFilter When non-empty (e.g. "FUNDING"), only shows configs with that exact
    /// curve_family_role; empty means any role.
    BootstrapConfigPickerDialog(ClientManager* clientManager,
                                QWidget* parent = nullptr,
                                const QString& currencyFilter = QString(),
                                const QString& roleFilter = "FUNDING");

    [[nodiscard]] std::optional<refdata::domain::ir_curve_bootstrap_config> selectedConfig() const {
        return selected_;
    }

private slots:
    void reload();
    void onRowActivated(int row);
    void onSelectClicked();

private:
    ClientManager* clientManager_;
    QString currencyFilter_;
    QString roleFilter_;
    QTableWidget* table_ = nullptr;
    QPushButton* selectButton_ = nullptr;
    QLabel* statusLabel_ = nullptr;

    std::vector<refdata::domain::ir_curve_bootstrap_config> rows_;
    std::optional<refdata::domain::ir_curve_bootstrap_config> selected_;
};

}

#endif
