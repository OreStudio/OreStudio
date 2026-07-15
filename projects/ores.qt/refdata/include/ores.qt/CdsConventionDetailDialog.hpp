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
#ifndef ORES_QT_CDS_CONVENTION_DETAIL_DIALOG_HPP
#define ORES_QT_CDS_CONVENTION_DETAIL_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include <vector>


namespace Ui {
class CdsConventionDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing CDS convention records.
 *
 * This dialog allows viewing, creating, and editing CDS conventions.
 * It supports both create mode (for new records) and edit mode (for
 * existing records).
 */
class CdsConventionDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.cds_convention_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CdsConventionDetailDialog(QWidget* parent = nullptr);
    ~CdsConventionDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setConvention(const refdata::domain::cds_convention& cc);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

    /**
     * @brief Force the dialog into the unsaved-changes state.
     *
     * Used when values are loaded programmatically and must be savable
     * immediately even though the user typed nothing — e.g. a revert, where
     * the act of loading a past version's values is itself the change.
     */
    void markDirty();


signals:
    void ccSaved(const QString& code);
    void ccDeleted(const QString& code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override {
        return hasChanges_;
    }
    QString code() const override;

private:
    void setupUi();
    void setupConnections();
    void updateUiFromConvention();
    void updateConventionFromUi();
    void updateSaveButtonState();
    bool validateInput();


    Ui::CdsConventionDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    refdata::domain::cds_convention cc_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};
};

}

#endif
