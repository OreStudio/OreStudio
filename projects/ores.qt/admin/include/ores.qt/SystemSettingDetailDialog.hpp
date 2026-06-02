/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_SYSTEM_SETTING_DETAIL_DIALOG_HPP
#define ORES_QT_SYSTEM_SETTING_DETAIL_DIALOG_HPP

#include <vector>
#include <QAction>
#include <QIntValidator>
#include <QToolBar>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"

namespace Ui {
class SystemSettingDetailDialog;
}

namespace ores::qt {

/**
 * @brief Dialog widget for creating and editing system settings.
 *
 * This widget provides a form for entering system setting details,
 * with save and delete capabilities.
 */
class SystemSettingDetailDialog : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.system_setting_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SystemSettingDetailDialog(QWidget* parent = nullptr);
    ~SystemSettingDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);

    void setSystemSetting(const variability::domain::system_setting& flag);
    variability::domain::system_setting getSystemSetting() const;
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly, int versionNumber = 0);
    void setHistory(const std::vector<variability::domain::system_setting>& history,
                    int versionNumber);
    void clearDialog();
    void save();

    QString systemSettingName() const;
    bool isDirty() const { return isDirty_; }
    bool isReadOnly() const { return isReadOnly_; }

signals:
    void isDirtyChanged(bool dirty);
    void systemSettingSaved(const QString& name);
    void systemSettingDeleted(const QString& name);

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override { return isDirty_; }

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onFieldChanged();
    void onDataTypeChanged();

    // Version navigation slots
    void onFirstVersionClicked();
    void onPrevVersionClicked();
    void onNextVersionClicked();
    void onLastVersionClicked();

private:
    void updateSaveButtonState();
    void closeParentWindow();
    void displayCurrentVersion();
    void updateVersionNavButtonStates();
    void showVersionNavActions(bool visible);
    void populateValueWidgets(const std::string& dataType, const std::string& value);
    void switchValuePage(const QString& dataType);
    [[nodiscard]] QString currentValueText() const;

private:
    Ui::SystemSettingDetailDialog* ui_;
    QToolBar* toolBar_;
    QAction* revertAction_;
    QIntValidator* intValidator_;

    variability::domain::system_setting currentFlag_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    std::string modifiedByUsername_;
    ClientManager* clientManager_;

    // Version navigation members
    std::vector<variability::domain::system_setting> history_;
    int currentHistoryIndex_;
    QAction* firstVersionAction_;
    QAction* prevVersionAction_;
    QAction* nextVersionAction_;
    QAction* lastVersionAction_;
};

}

#endif
