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
#ifndef ORES_QT_CONNECTION_DETAIL_DIALOG_HPP
#define ORES_QT_CONNECTION_DETAIL_DIALOG_HPP

#include <QDialog>
#include <QLineEdit>
#include <QSpinBox>
#include <QComboBox>
#include <QTextEdit>
#include <QPushButton>
#include <QDialogButtonBox>
#include <QCheckBox>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/server_environment.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

/**
 * @brief Dialog for creating and editing server environments (connections).
 */
class ConnectionDetailDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.connection_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ConnectionDetailDialog(
        connections::service::connection_manager* manager,
        QWidget* parent = nullptr);
    ~ConnectionDetailDialog() override;

    /**
     * @brief Set existing environment for editing.
     */
    void setEnvironment(const connections::domain::server_environment& env);

    /**
     * @brief Set initial folder for new environment.
     */
    void setInitialFolder(const std::optional<boost::uuids::uuid>& folderId);

    /**
     * @brief Get the environment data from the dialog.
     */
    connections::domain::server_environment getEnvironment() const;

    /**
     * @brief Get the password (only set if changed).
     */
    std::optional<std::string> getPassword() const;

    /**
     * @brief Check if this is a new environment (add mode).
     */
    bool isAddMode() const { return isAddMode_; }

private slots:
    void onSaveClicked();
    void updateSaveButtonState();
    void onPasswordChanged();
    void togglePasswordVisibility();

private:
    void setupUI();
    void populateFolderCombo();
    bool validateInput();

    connections::service::connection_manager* manager_;

    QLineEdit* nameEdit_;
    QLineEdit* hostEdit_;
    QSpinBox* portSpinBox_;
    QLineEdit* usernameEdit_;
    QLineEdit* passwordEdit_;
    QCheckBox* showPasswordCheckbox_;
    QTextEdit* descriptionEdit_;
    QComboBox* folderCombo_;
    QDialogButtonBox* buttonBox_;

    bool isAddMode_{true};
    bool passwordChanged_{false};
    boost::uuids::uuid environmentId_;
};

}

#endif
