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
#ifndef ORES_QT_ORG_CHART_WIDGET_HPP
#define ORES_QT_ORG_CHART_WIDGET_HPP

#include "ores.iam.api/domain/account.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/IamExport.hpp"
#include <QGraphicsItemGroup>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QWidget>
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::qt {

class ClientManager;
class ImageCache;
class ClientAccountModel;

/**
 * @brief Read-only, auto-laid-out org chart for the current tenant's staff.
 *
 * Fetches every account for the current tenant and renders one card per
 * account (photo, full name, job title, party/office label), connected by
 * lines to its manager (account.reports_to_account_id). Layout is a simple
 * layered/centred tree pass -- no manual positioning, no saved layout
 * state; re-fetching and re-rendering from scratch is cheap enough at
 * staff-directory scale (tens, not thousands, of accounts) that there is
 * no incremental-update path. A regular QWidget (not QDialog) so it embeds
 * as a normal, maximizable MDI subwindow like every other list window.
 */
class ORES_QT_IAM_EXPORT OrgChartWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.org_chart_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OrgChartWidget(ClientManager* clientManager,
                            ImageCache* imageCache,
                            QWidget* parent = nullptr);
    ~OrgChartWidget() override;

private slots:
    void onAccountsLoaded();
    void onImageLoaded(const QString& imageId);

private:
    /** @brief One account's position in the tree, built from the flat account list. */
    struct Node {
        iam::domain::account account;
        std::vector<Node*> children;
        double x = 0;
        int depth = 0;
        int colourIndex = 0;
        std::string partyLabel;
        QGraphicsItemGroup* cardItem = nullptr;
        QGraphicsPixmapItem* photoItem = nullptr;
    };

    void rebuildTree();
    void resolvePartyLabels();
    double layoutSubtree(Node& node, double& cursor);
    void renderScene();
    QGraphicsItemGroup* createCard(Node& node);
    QPixmap circularPhoto(const boost::uuids::uuid& imageId, int diameter) const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    std::unique_ptr<ClientAccountModel> accountModel_;

    QGraphicsScene* scene_;
    QGraphicsView* view_;

    std::vector<std::unique_ptr<Node>> nodes_;
    std::unordered_map<std::string, Node*> nodesByImageId_;
    std::vector<Node*> roots_;
};

}

#endif
