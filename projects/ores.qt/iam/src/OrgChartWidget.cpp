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
#include "ores.qt/OrgChartWidget.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.qt/ClientAccountModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <QFont>
#include <QFutureWatcher>
#include <QGraphicsLineItem>
#include <QGraphicsPathItem>
#include <QGraphicsPixmapItem>
#include <QGraphicsSimpleTextItem>
#include <QGraphicsTextItem>
#include <QPainter>
#include <QPainterPath>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <functional>

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr int CARD_W = 210;
constexpr int CARD_H = 122;
constexpr int H_GAP = 26;
constexpr int V_GAP = 56;
constexpr int PHOTO_DIAM = 44;
constexpr int TREE_GAP = 2; // extra slot-widths between independent root trees
constexpr int ROOT_COLOUR_INDEX = -1;
constexpr int TEXT_X = 12 + PHOTO_DIAM + 10;
constexpr int TEXT_W = CARD_W - TEXT_X - 10;

// True organisational roots (no manager at all -- the group/holding level)
// get this neutral, dark colour so they read as "above" every coloured
// branch, with white text. Every other colour comes from cardPalette()
// below and is assigned per depth-1 branch (e.g. one colour per office),
// reused down that branch's whole subtree, with dark text since the
// palette is pastel/light.
const QColor& rootColour() {
    static const QColor colour(0x46, 0x52, 0x63);
    return colour;
}

const QVector<QColor>& cardPalette() {
    static const QVector<QColor> colours{
        QColor(0xe6, 0xc2, 0xcf), // pastel mauve
        QColor(0xc3, 0xe6, 0xd2), // pastel green
        QColor(0xf5, 0xd9, 0xb0), // pastel peach
        QColor(0xbf, 0xe3, 0xe3), // pastel teal
        QColor(0xc7, 0xd7, 0xf0), // pastel blue
        QColor(0xdc, 0xc9, 0xef), // pastel purple
    };
    return colours;
}

const QColor& currentUserHighlightColour() {
    static const QColor colour(0xd4, 0x37, 0x2b);
    return colour;
}

} // namespace

OrgChartWidget::OrgChartWidget(ClientManager* clientManager, ImageCache* imageCache, QWidget* parent)
    : QWidget(parent), clientManager_(clientManager), imageCache_(imageCache) {
    resize(1000, 700);

    scene_ = new QGraphicsScene(this);
    view_ = new QGraphicsView(scene_, this);
    view_->setRenderHint(QPainter::Antialiasing);
    view_->setDragMode(QGraphicsView::ScrollHandDrag);

    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(view_);

    accountModel_ = std::make_unique<ClientAccountModel>(clientManager_, this);
    connect(accountModel_.get(), &ClientAccountModel::dataLoaded, this,
            &OrgChartWidget::onAccountsLoaded);
    connect(accountModel_.get(), &ClientAccountModel::loadError, this,
            [](const QString& message, const QString&) {
                BOOST_LOG_SEV(lg(), error) << "Failed to load accounts for org chart: "
                                            << message.toStdString();
            });

    if (imageCache_) {
        connect(imageCache_, &ImageCache::imageLoaded, this, &OrgChartWidget::onImageLoaded);
    }

    accountModel_->refresh(true);
}

OrgChartWidget::~OrgChartWidget() = default;

void OrgChartWidget::onAccountsLoaded() {
    rebuildTree();

    double cursor = 0;
    for (Node* root : roots_) {
        layoutSubtree(*root, cursor);
        cursor += TREE_GAP;
    }

    resolvePartyLabels();
}

void OrgChartWidget::rebuildTree() {
    nodes_.clear();
    nodesByImageId_.clear();
    roots_.clear();

    const auto accounts = accountModel_->getAccountsWithLoginInfo();

    std::unordered_map<std::string, Node*> byAccountId;
    nodes_.reserve(accounts.size());
    for (const auto& entry : accounts) {
        // Accounts with no display name (e.g. the bootstrap tenant_admin,
        // created outside the staff-generation pipeline) aren't real staff
        // -- skip them rather than rendering a blank, unlabelled card.
        if (entry.account.full_name.empty())
            continue;

        auto node = std::make_unique<Node>();
        node->account = entry.account;
        Node* raw = node.get();
        byAccountId[boost::uuids::to_string(entry.account.id)] = raw;
        if (!entry.account.image_id.is_nil())
            nodesByImageId_[boost::uuids::to_string(entry.account.image_id)] = raw;
        nodes_.push_back(std::move(node));
    }

    for (auto& node : nodes_) {
        if (node->account.reports_to_account_id.is_nil()) {
            roots_.push_back(node.get());
            continue;
        }
        auto it = byAccountId.find(boost::uuids::to_string(node->account.reports_to_account_id));
        if (it == byAccountId.end()) {
            // Manager not present in this tenant's account list (shouldn't
            // normally happen) -- treat as a root rather than dropping the
            // account from the chart.
            roots_.push_back(node.get());
            continue;
        }
        it->second->children.push_back(node.get());
    }

    // Colour by branch, not by independent root: a true root (no manager --
    // group/holding level) gets the neutral rootColour(); each of a root's
    // direct children (e.g. one per office) gets its own palette colour,
    // reused down that child's whole subtree, so which office/branch a
    // person belongs to is visible at a glance rather than everyone under
    // one holding-company root sharing a single colour. A shared counter
    // keeps branch colours distinct across multiple independent roots too.
    int nextBranchColour = 0;
    std::function<void(Node&, int, int)> assign = [&](Node& n, int depth, int colour) {
        n.depth = depth;
        n.colourIndex = colour;
        for (Node* child : n.children) {
            const int childColour = depth == 0
                ? (nextBranchColour++ % static_cast<int>(cardPalette().size()))
                : colour;
            assign(*child, depth + 1, childColour);
        }
    };
    for (Node* root : roots_)
        assign(*root, 0, ROOT_COLOUR_INDEX);
}

void OrgChartWidget::resolvePartyLabels() {
    struct LoadResult {
        bool success = false;
        std::unordered_map<std::string, std::string> accountIdToPartyId;
        std::unordered_map<std::string, std::string> partyIdToName;
    };

    // One anchor per branch -- each root (group/holding level) plus each of
    // its direct children (one per office) -- rather than every account:
    // party affiliation is uniform across a whole branch's subtree, so
    // resolving it once per branch and propagating down is enough.
    std::vector<boost::uuids::uuid> anchorIds;
    for (Node* root : roots_) {
        anchorIds.push_back(root->account.id);
        for (Node* child : root->children)
            anchorIds.push_back(child->account.id);
    }

    if (anchorIds.empty() || !clientManager_) {
        renderScene();
        return;
    }

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this, [this, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            auto labelFor = [&](Node& anchor) -> std::string {
                auto pit = result.accountIdToPartyId.find(boost::uuids::to_string(anchor.account.id));
                if (pit == result.accountIdToPartyId.end())
                    return {};
                auto nit = result.partyIdToName.find(pit->second);
                return nit != result.partyIdToName.end() ? nit->second : std::string{};
            };

            std::function<void(Node&, const std::string&)> propagate =
                [&](Node& n, const std::string& label) {
                    n.partyLabel = label;
                    for (Node* child : n.children)
                        propagate(*child, label);
                };

            for (Node* root : roots_) {
                root->partyLabel = labelFor(*root);
                for (Node* child : root->children)
                    propagate(*child, labelFor(*child));
            }
        }

        renderScene();
    });

    auto* clientManager = clientManager_;
    QFuture<LoadResult> future = QtConcurrent::run([clientManager, anchorIds]() -> LoadResult {
        LoadResult result;

        refdata::messaging::get_parties_request partiesRequest;
        partiesRequest.limit = 1000;
        auto partiesResult =
            clientManager->process_authenticated_request(std::move(partiesRequest));
        if (!partiesResult) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to fetch parties for org chart: " << partiesResult.error();
            return result;
        }
        for (const auto& party : partiesResult->parties)
            result.partyIdToName[boost::uuids::to_string(party.id)] = party.full_name;

        for (const auto& accountId : anchorIds) {
            iam::messaging::get_account_parties_by_account_request request;
            request.account_id = boost::uuids::to_string(accountId);
            auto accountPartiesResult =
                clientManager->process_authenticated_request(std::move(request));
            if (accountPartiesResult && !accountPartiesResult->account_parties.empty()) {
                result.accountIdToPartyId[boost::uuids::to_string(accountId)] =
                    boost::uuids::to_string(
                        accountPartiesResult->account_parties.front().party_id);
            }
        }

        result.success = true;
        return result;
    });

    watcher->setFuture(future);
}

double OrgChartWidget::layoutSubtree(Node& node, double& cursor) {
    if (node.children.empty()) {
        node.x = cursor;
        cursor += 1.0;
        return node.x;
    }

    double sum = 0;
    for (Node* child : node.children)
        sum += layoutSubtree(*child, cursor);
    node.x = sum / static_cast<double>(node.children.size());
    return node.x;
}

void OrgChartWidget::renderScene() {
    scene_->clear();

    for (auto& node : nodes_) {
        node->cardItem = createCard(*node);
        const double px = node->x * (CARD_W + H_GAP);
        const double py = node->depth * (CARD_H + V_GAP);
        node->cardItem->setPos(px, py);
    }

    for (auto& node : nodes_) {
        for (Node* child : node->children) {
            const QPointF from = node->cardItem->pos() + QPointF(CARD_W / 2.0, CARD_H);
            const QPointF to = child->cardItem->pos() + QPointF(CARD_W / 2.0, 0);
            auto* line = scene_->addLine(QLineF(from, to), QPen(QColor(0x90, 0x90, 0x90), 2));
            line->setZValue(-1);
        }
    }

    scene_->setSceneRect(scene_->itemsBoundingRect().adjusted(-40, -40, 40, 40));
}

QGraphicsItemGroup* OrgChartWidget::createCard(Node& node) {
    auto* group = new QGraphicsItemGroup();

    const bool isRoot = node.colourIndex == ROOT_COLOUR_INDEX;
    const QColor background_colour = isRoot ? rootColour() : cardPalette()[node.colourIndex];
    const QColor name_colour = isRoot ? Qt::white : QColor(0x2b, 0x2b, 0x2b);
    const QColor title_colour = isRoot ? QColor(255, 255, 255, 210) : QColor(0x2b, 0x2b, 0x2b, 200);
    const QColor party_colour = isRoot ? QColor(255, 255, 255, 170) : QColor(0x2b, 0x2b, 0x2b, 150);

    const bool isCurrentUser =
        clientManager_ && !node.account.username.empty()
        && node.account.username == clientManager_->currentUsername();

    QPainterPath path;
    path.addRoundedRect(QRectF(0, 0, CARD_W, CARD_H), 14, 14);
    auto* background = new QGraphicsPathItem(path);
    background->setPen(isCurrentUser ? QPen(currentUserHighlightColour(), 3) : Qt::NoPen);
    background->setBrush(background_colour);
    group->addToGroup(background);

    auto* photo = new QGraphicsPixmapItem(circularPhoto(node.account.image_id, PHOTO_DIAM));
    photo->setPos(12, (CARD_H - PHOTO_DIAM) / 2.0);
    group->addToGroup(photo);
    node.photoItem = photo;

    QFont nameFont;
    nameFont.setBold(true);
    nameFont.setPointSize(10);
    auto* nameText = new QGraphicsSimpleTextItem(
        QString::fromStdString(node.account.full_name));
    nameText->setFont(nameFont);
    nameText->setBrush(name_colour);
    nameText->setPos(TEXT_X, 10);
    group->addToGroup(nameText);

    constexpr qreal TITLE_Y = 30;

    QFont titleFont;
    titleFont.setPointSize(8);
    auto* titleText = new QGraphicsTextItem(QString::fromStdString(node.account.job_title));
    titleText->setFont(titleFont);
    titleText->setDefaultTextColor(title_colour);
    titleText->setTextWidth(TEXT_W);
    titleText->setPos(TEXT_X - 4, TITLE_Y);
    group->addToGroup(titleText);

    if (!node.partyLabel.empty()) {
        // Positioned right below the title's own (word-wrapped, 1-2 line)
        // rendered height rather than a fixed offset, so short and long
        // titles don't leave a mismatched gap before the party label.
        QFont partyFont;
        partyFont.setPointSize(7);
        partyFont.setItalic(true);
        auto* partyText = new QGraphicsTextItem(QString::fromStdString(node.partyLabel));
        partyText->setFont(partyFont);
        partyText->setDefaultTextColor(party_colour);
        partyText->setTextWidth(TEXT_W);
        partyText->setPos(TEXT_X - 4, TITLE_Y + titleText->boundingRect().height() - 4);
        group->addToGroup(partyText);
    }

    scene_->addItem(group);
    return group;
}

QPixmap OrgChartWidget::circularPhoto(const boost::uuids::uuid& imageId, int diameter) const {
    QPixmap source;
    if (!imageId.is_nil() && imageCache_) {
        source = imageCache_->getIcon(boost::uuids::to_string(imageId)).pixmap(diameter, diameter);
    }

    QPixmap result(diameter, diameter);
    result.fill(Qt::transparent);
    QPainter painter(&result);
    painter.setRenderHint(QPainter::Antialiasing);

    QPainterPath clip;
    clip.addEllipse(0, 0, diameter, diameter);
    painter.setClipPath(clip);

    if (!source.isNull()) {
        painter.drawPixmap(
            0, 0, diameter, diameter,
            source.scaled(diameter, diameter, Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation));
    } else {
        painter.fillRect(0, 0, diameter, diameter, QColor(255, 255, 255, 60));
    }

    return result;
}

void OrgChartWidget::onImageLoaded(const QString& imageId) {
    auto it = nodesByImageId_.find(imageId.toStdString());
    if (it == nodesByImageId_.end() || !it->second->photoItem)
        return;
    it->second->photoItem->setPixmap(circularPhoto(it->second->account.image_id, PHOTO_DIAM));
}

}
