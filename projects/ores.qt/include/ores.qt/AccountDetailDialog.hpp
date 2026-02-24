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
#ifndef ORES_QT_ACCOUNT_DETAIL_DIALOG_HPP
#define ORES_QT_ACCOUNT_DETAIL_DIALOG_HPP

#include <QToolBar>
#include <QAction>
#include <memory>
#include <optional>
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/login_info.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/AccountRolesWidget.hpp"
#include "ores.qt/AccountPartiesWidget.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"


namespace Ui {

class AccountDetailDialog;

}

namespace ores::qt {

/**
 * @brief Dialog widget for creating and editing user accounts.
 *
 * In create mode (new account):
 * - Username, email, and password fields are editable
 * - Password and confirmation are required
 *
 * In edit mode (existing account):
 * - Username is read-only
 * - Password fields are hidden (password changes not supported yet)
 * - Email and admin status can be modified
 */
class AccountDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountDetailDialog(QWidget* parent = nullptr);
    ~AccountDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setChangeReasonCache(ChangeReasonCache* cache);
    void setUsername(const std::string& username);

    /**
     * @brief Sets the account to display/edit.
     *
     * If account has an empty username, dialog enters create mode.
     * Otherwise, dialog enters edit mode with username read-only.
     *
     * @param account The account to display/edit
     */
    void setAccount(const iam::domain::account& account);

    /**
     * @brief Sets the login info to display (read-only).
     *
     * This populates the Login Status group with the current login
     * status information for the account.
     *
     * @param loginInfo The login info to display, or nullopt to clear
     */
    void setLoginInfo(const std::optional<iam::domain::login_info>& loginInfo);

    /**
     * @brief Gets the current account state from the form.
     *
     * @return The account with values from the form fields
     */
    [[nodiscard]] iam::domain::account getAccount() const;

    /**
     * @brief Clears all form fields and resets dialog state.
     */
    void clearDialog();

    /**
     * @brief Saves the account (create or update).
     */
    void save();

    /**
     * @brief Sets the dialog to read-only mode for viewing historical versions.
     *
     * In read-only mode:
     * - All fields are disabled
     * - Save button is hidden
     * - Delete button is hidden
     * - Revert button is shown
     * - Toolbar shows version information
     *
     * @param readOnly True to enable read-only mode
     * @param versionNumber The historical version number being displayed
     */
    void setReadOnly(bool readOnly, int versionNumber = 0);

    /**
     * @brief Mark the dialog data as stale.
     *
     * Called when a notification is received indicating this account has
     * changed on the server. Shows a visual indicator that the data may be
     * out of date.
     */
    void markAsStale();

    /**
     * @brief Returns the account ID of the account being edited.
     */
    [[nodiscard]] QString accountId() const;

signals:
    void accountUpdated(const boost::uuids::uuid& account_id);
    void accountCreated(const boost::uuids::uuid& account_id);
    void accountDeleted(const boost::uuids::uuid& account_id);
    void isDirtyChanged(bool isDirty);

    /**
     * @brief Emitted when user requests to revert to the displayed historical version.
     * @param account The account data to revert to.
     */
    void revertRequested(const iam::domain::account& account);

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;

private slots:
    void onSaveClicked();
    void onResetClicked();
    void onDeleteClicked();
    void onRevertClicked();
    void onFieldChanged();

private:
    void updateSaveResetButtonState();
    void setCreateMode(bool createMode);
    void setFieldsReadOnly(bool readOnly);
    bool validatePassword() const;

private:
    std::unique_ptr<Ui::AccountDetailDialog> ui_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    bool isStale_;
    int historicalVersion_;
    std::string modifiedByUsername_;
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* deleteAction_;
    QAction* revertAction_;

    ClientManager*     clientManager_;
    ChangeReasonCache* changeReasonCache_ = nullptr;
    iam::domain::account currentAccount_;
    std::optional<iam::domain::login_info> currentLoginInfo_;
    AccountRolesWidget*   rolesWidget_;
    AccountPartiesWidget* partiesWidget_;
};

}

#endif
