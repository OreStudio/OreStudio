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
#ifndef ORES_QT_FEATURE_FLAG_DETAIL_DIALOG_HPP
#define ORES_QT_FEATURE_FLAG_DETAIL_DIALOG_HPP

#include <vector>
#include <QWidget>
#include <QAction>
#include <QToolBar>
#include "ores.qt/ClientManager.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"

namespace Ui {
class FeatureFlagDetailDialog;
}

namespace ores::qt {

/**
 * @brief Dialog widget for creating and editing feature flags.
 *
 * This widget provides a form for entering feature flag details,
 * with save and delete capabilities.
 */
class FeatureFlagDetailDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.feature_flag_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit FeatureFlagDetailDialog(QWidget* parent = nullptr);
    ~FeatureFlagDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);

    void setFeatureFlag(const variability::domain::feature_flags& flag);
    variability::domain::feature_flags getFeatureFlag() const;
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly, int versionNumber = 0);
    void setHistory(const std::vector<variability::domain::feature_flags>& history,
                    int versionNumber);
    void clearDialog();
    void save();

    QString featureFlagName() const;
    bool isDirty() const { return isDirty_; }
    bool isReadOnly() const { return isReadOnly_; }

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void isDirtyChanged(bool dirty);
    void featureFlagSaved(const QString& name);
    void featureFlagDeleted(const QString& name);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onFieldChanged();

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

private:
    Ui::FeatureFlagDetailDialog* ui_;
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* deleteAction_;
    QAction* revertAction_;

    variability::domain::feature_flags currentFlag_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    std::string modifiedByUsername_;
    ClientManager* clientManager_;

    // Version navigation members
    std::vector<variability::domain::feature_flags> history_;
    int currentHistoryIndex_;
    QAction* firstVersionAction_;
    QAction* prevVersionAction_;
    QAction* nextVersionAction_;
    QAction* lastVersionAction_;
};

}

#endif
