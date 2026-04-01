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
#ifndef ORES_QT_COMPOSITE_INSTRUMENT_DETAIL_DIALOG_HPP
#define ORES_QT_COMPOSITE_INSTRUMENT_DETAIL_DIALOG_HPP

#include <vector>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"

namespace Ui {
class CompositeInstrumentDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing composite instrument records.
 */
class CompositeInstrumentDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.composite_instrument_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CompositeInstrumentDetailDialog(QWidget* parent = nullptr);
    ~CompositeInstrumentDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setCompositeInstrument(
        const trading::domain::composite_instrument& v);
    void setCompositeInstrumentWithLegs(
        const trading::domain::composite_instrument& v,
        const std::vector<trading::domain::composite_leg>& legs);
    void setLegs(
        const std::vector<trading::domain::composite_leg>& legs);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

    /**
     * @brief Asynchronously fetch legs from the server and populate the widget.
     *
     * Call after setCompositeInstrument() when opening an existing record.
     */
    void loadLegs();

signals:
    void compositeInstrumentSaved(const QString& id);
    void compositeInstrumentDeleted(const QString& id);

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
    void updateUiFromCompositeInstrument();
    void updateCompositeInstrumentFromUi();
    void updateSaveButtonState();
    bool validateInput();

    Ui::CompositeInstrumentDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    trading::domain::composite_instrument instrument_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};
};

}

#endif
