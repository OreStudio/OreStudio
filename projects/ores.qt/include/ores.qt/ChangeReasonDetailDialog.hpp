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
#ifndef ORES_QT_CHANGE_REASON_DETAIL_DIALOG_HPP
#define ORES_QT_CHANGE_REASON_DETAIL_DIALOG_HPP

#include <vector>
#include <QAction>
#include <QToolBar>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/change_reason.hpp"
#include "ores.dq/domain/change_reason_category.hpp"

namespace Ui {
class ChangeReasonDetailDialog;
}

namespace ores::qt {

/**
 * @brief Dialog widget for creating and editing change reasons.
 *
 * This widget provides a form for entering change reason details,
 * with save and delete capabilities.
 */
class ChangeReasonDetailDialog : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.change_reason_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ChangeReasonDetailDialog(QWidget* parent = nullptr);
    ~ChangeReasonDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);

    void setChangeReason(const dq::domain::change_reason& reason);
    dq::domain::change_reason getChangeReason() const;
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly, int versionNumber = 0);
    void setHistory(const std::vector<dq::domain::change_reason>& history,
                    int versionNumber);
    void clearDialog();
    void save();

    void setCategories(const std::vector<dq::domain::change_reason_category>& categories);

    QString changeReasonCode() const;
    bool isDirty() const { return isDirty_; }
    bool isReadOnly() const { return isReadOnly_; }

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void isDirtyChanged(bool dirty);
    void changeReasonSaved(const QString& code);
    void changeReasonDeleted(const QString& code);

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
    void populateCategoryComboBox();

private:
    Ui::ChangeReasonDetailDialog* ui_;
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* deleteAction_;
    QAction* revertAction_;

    dq::domain::change_reason currentReason_;
    std::vector<dq::domain::change_reason_category> categories_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    std::string modifiedByUsername_;
    ClientManager* clientManager_;

    // Version navigation members
    std::vector<dq::domain::change_reason> history_;
    int currentHistoryIndex_;
    QAction* firstVersionAction_;
    QAction* prevVersionAction_;
    QAction* nextVersionAction_;
    QAction* lastVersionAction_;
};

}

#endif
