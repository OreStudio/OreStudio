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
#ifndef ORES_QT_ADD_ITEM_DIALOG_HPP
#define ORES_QT_ADD_ITEM_DIALOG_HPP

#include <QWidget>
#include <QToolBar>
#include <QAction>
#include <QComboBox>
#include <QLineEdit>
#include <QSpinBox>
#include <QTextEdit>
#include <QCheckBox>
#include <QLabel>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/domain/server_environment.hpp"
#include "ores.connections/domain/tag.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ConnectionTypes.hpp"

namespace ores::qt {
class TagSelectorWidget;
}

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

/**
 * @brief Item type selector for the combined add dialog.
 */
enum class ItemType {
    Folder,
    Connection
};

/**
 * @brief Combined modeless dialog for creating and editing folders and connections.
 *
 * This widget provides a unified form for both item types, with fields
 * dynamically enabled/disabled based on the selected type.
 */
class AddItemDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.add_item_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AddItemDialog(
        connections::service::connection_manager* manager,
        QWidget* parent = nullptr);
    ~AddItemDialog() override;

    void setItemType(ItemType type);
    ItemType itemType() const { return itemType_; }

    void setCreateMode(bool createMode);
    bool isCreateMode() const { return isCreateMode_; }

    // Folder operations
    void setFolder(const connections::domain::folder& folder);
    connections::domain::folder getFolder() const;
    void setInitialParent(const std::optional<boost::uuids::uuid>& parentId);

    // Connection operations
    void setEnvironment(const connections::domain::server_environment& env);
    connections::domain::server_environment getEnvironment() const;
    void setInitialFolder(const std::optional<boost::uuids::uuid>& folderId);
    std::optional<std::string> getPassword() const;

    // Tags (connection only)
    void setTags(const std::vector<connections::domain::tag>& tags);
    std::vector<boost::uuids::uuid> getSelectedTagIds() const;

    // Test callback (connection only)
    void setTestCallback(TestConnectionCallback callback);

    QString itemName() const;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void folderSaved(const boost::uuids::uuid& id, const QString& name);
    void connectionSaved(const boost::uuids::uuid& id, const QString& name);

private slots:
    void onSaveClicked();
    void onTestClicked();
    void onTypeChanged(int index);
    void onPasswordChanged();
    void togglePasswordVisibility();
    void updateSaveButtonState();

private:
    void setupUI();
    void setupToolbar();
    void populateFolderCombo();
    void updateFieldVisibility();
    bool validateInput();
    void saveFolder();
    void saveConnection();

    connections::service::connection_manager* manager_;

    // Toolbar
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* testAction_;

    // Type selector
    QComboBox* typeCombo_;

    // Common fields
    QLineEdit* nameEdit_;
    QComboBox* folderCombo_;
    QTextEdit* descriptionEdit_;

    // Connection-only fields
    QLineEdit* hostEdit_;
    QSpinBox* portSpinBox_;
    QLineEdit* usernameEdit_;
    QLineEdit* passwordEdit_;
    QCheckBox* showPasswordCheckbox_;
    TagSelectorWidget* tagSelector_;

    // Labels for showing/hiding
    QLabel* hostLabel_;
    QLabel* portLabel_;
    QLabel* usernameLabel_;
    QLabel* passwordLabel_;
    QLabel* tagLabel_;
    QWidget* passwordWidget_;

    // State
    ItemType itemType_{ItemType::Connection};
    bool isCreateMode_{true};
    bool passwordChanged_{false};

    // IDs for edit mode
    boost::uuids::uuid folderId_;
    boost::uuids::uuid environmentId_;

    TestConnectionCallback testCallback_;
};

}

#endif
