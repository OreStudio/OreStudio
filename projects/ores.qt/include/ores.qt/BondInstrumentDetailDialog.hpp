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
#ifndef ORES_QT_BOND_INSTRUMENT_DETAIL_DIALOG_HPP
#define ORES_QT_BOND_INSTRUMENT_DETAIL_DIALOG_HPP

#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"

namespace Ui {
class BondInstrumentDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing bond instrument records.
 */
class BondInstrumentDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.bond_instrument_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BondInstrumentDetailDialog(QWidget* parent = nullptr);
    ~BondInstrumentDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setBondInstrument(const trading::domain::bond_instrument& v);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void bondInstrumentSaved(const QString& id);
    void bondInstrumentDeleted(const QString& id);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onFieldChanged();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override { return hasChanges_; }

private:
    void setupUi();
    void setupConnections();
    void updateUiFromBondInstrument();
    void updateBondInstrumentFromUi();
    void updateSaveButtonState();
    bool validateInput();

    Ui::BondInstrumentDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    trading::domain::bond_instrument instrument_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};
};

}

#endif
