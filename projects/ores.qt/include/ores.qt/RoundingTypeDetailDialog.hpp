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
#ifndef ORES_QT_ROUNDING_TYPE_DETAIL_DIALOG_HPP
#define ORES_QT_ROUNDING_TYPE_DETAIL_DIALOG_HPP

#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/rounding_type.hpp"

namespace Ui {
class RoundingTypeDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing rounding type records.
 *
 * This dialog allows viewing, creating, and editing rounding types.
 * It supports both create mode (for new records) and edit mode (for
 * existing records).
 */
class RoundingTypeDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.rounding_type_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit RoundingTypeDetailDialog(QWidget* parent = nullptr);
    ~RoundingTypeDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setType(const refdata::domain::rounding_type& type);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void typeSaved(const QString& code);
    void typeDeleted(const QString& code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;

private:
    void setupUi();
    void setupConnections();
    void updateUiFromType();
    void updateTypeFromUi();
    void updateSaveButtonState();
    bool validateInput();

    Ui::RoundingTypeDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    refdata::domain::rounding_type type_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};
};

}

#endif
